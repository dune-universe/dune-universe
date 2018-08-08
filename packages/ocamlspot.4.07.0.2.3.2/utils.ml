(***********************************************************************)
(*                                                                     *)
(*                            OCamlSpotter                             *)
(*                                                                     *)
(*                             Jun FURUSE                              *)
(*                                                                     *)
(*   Copyright 2008-2014 Jun Furuse. All rights reserved.              *)
(*   This file is distributed under the terms of the GNU Library       *)
(*   General Public License, with the special exception on linking     *)
(*   described in file LICENSE.                                        *)
(*                                                                     *)
(***********************************************************************)

open Format

module List = struct
  include List

  let rec find_map_opt f = function
    | [] -> None
    | x::xs ->
        match f x with
        | Some v -> Some v
        | None -> find_map_opt f xs

  let filter_map f lst =
    List.rev (List.fold_left (fun st x ->
      match f x with
      | Some v -> v :: st
      | None -> st) [] lst)

  (** concatMap of Haskell *)
  let concat_map f l = List.concat (List.map f l)
end

module Debug = struct
  let on = ref false

  let format fmt = 
    if !on then eprintf fmt
    else Format.ifprintf Format.err_formatter fmt
end

module Lazy = struct
  include Lazy

  module Open = struct
    let (!!) = Lazy.force 
    let eager = Lazy.from_val
  end

  open Open

  let peek v = if is_val v then Some (!!v) else None
      
  let apply f v = 
    if is_val v then eager (f !!v)
    else lazy (f !!v)

end

include Lazy.Open

module String = struct
  include String

  (* split a string according to char_sep predicate *)
  let split char_sep str =
    let len = String.length str in
    if len = 0 then [] else
      let rec skip_sep cur =
        if cur >= len then cur
        else if char_sep str.[cur] then skip_sep (succ cur)
        else cur  in
      let rec split beg cur =
        if cur >= len then 
  	if beg = cur then []
  	else [String.sub str beg (len - beg)]
        else if char_sep str.[cur] 
  	   then 
  	     let nextw = skip_sep cur in
  	      (String.sub str beg (cur - beg))
  		::(split nextw nextw)
  	   else split beg (succ cur) in
      let wstart = skip_sep 0 in
      split wstart wstart


  (** Same as [String.sub] but even if the string shorter for [len] 
      the function succeeds and returns a shorter substring. 
  *)
  let sub' s pos len =
    let orig_len = length s in
    let len = max (min (pos + len) orig_len - pos) 0 in
    sub s pos len

  let split_at s pos = sub s 0 pos, sub s pos (length s - pos)

  let find s pos f =
    let len = length s in
    let rec scan pos =
      if pos >= len then None
      else if f (unsafe_get s pos) then Some pos else scan (pos + 1)
    in
    scan pos

  let replace_chars from to_ s =
    let open Bytes in
    let s' = copy (of_string s) in
    iteri (fun p -> function
      | c when c = from -> unsafe_set s' p to_
      | _ -> ()) s';
    to_string s'

end

module Filename = struct
  include Filename
      
  let split_extension s = 
    try
      let body = chop_extension s in
      body, 
      String.sub s 
	(String.length body) 
	(String.length s - String.length body)
    with
    | Invalid_argument _ -> s, ""

  let concats = String.concat dir_sep

  module Open = struct
    let (^/) p1 p2 =
      if Filename.is_relative p2 then Filename.concat p1 p2 else p2
  end

end

include Filename.Open

module Format = struct
  include Format
  let rec list (sep : (unit, formatter, unit) format)  f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t%a" 
	  f x
	  (fun ppf -> fprintf ppf sep)
	  (list sep f) xs

  let option f ppf = function
    | None -> fprintf ppf "None"
    | Some v -> fprintf ppf "Some(%a)" f v 

  let lazy_ p ppf v =
    if Lazy.is_val v then p ppf (Lazy.Open.(!!) v)
    else fprintf ppf "lazy"
end

module Option = struct
  let map ~f = function
    | None -> None
    | Some v -> Some (f v)

  let bind v f = match v with
    | None -> None
    | Some v -> f v

  let iter ~f = function
    | None -> ()
    | Some v -> f v

  let default v df = match v with
    | None -> df ()
    | Some v -> v
end

exception Finally of exn * exn
;;

let protect ~f x ~(finally : 'a -> unit) =
  let res =
    try f x with exn ->
      (try finally x with final_exn -> raise (Finally (exn, final_exn)));
    raise exn
  in
  finally x;
  res
;;

let protect' name f v = try f v with e ->
  Format.eprintf "Error: %s: %s@." name (Printexc.to_string e); raise e

let catch ~f v = try `Ok (f v) with e -> `Error e;;

let failwithf fmt = Printf.kprintf failwith fmt
let invalid_argf fmt = Printf.kprintf invalid_arg fmt

let with_ref r v f =
  let back_v = !r in
  r := v;
  protect ~f () ~finally:(fun () -> r := back_v)

module Unix = struct
  include Unix

  let kind path = try Some (Unix.stat path).st_kind with _ -> None

  let is_dir path = kind path = Some S_DIR

  let gen_timed get minus f v = 
    let t1 = get () in
    let res = f v  in
    let t2 = get () in
    res, minus t2 t1

  let timed f v = gen_timed Unix.gettimeofday (-.) f v

  let dev_inode path =
    try
      let st = Unix.lstat path in
      Some (st.Unix.st_dev, st.Unix.st_ino)
    with
    | _ -> None

  module Process_times = struct
    type t = process_times
    let (-) pt1 pt2 = {
      tms_utime  = pt1.tms_utime -. pt2.tms_utime;
      tms_stime  = pt1.tms_stime -. pt2.tms_stime;
      tms_cutime = pt1.tms_utime -. pt2.tms_cutime;
      tms_cstime = pt1.tms_utime -. pt2.tms_cstime;
    }
    let timed f v = gen_timed Unix.times (-) f v
  end
  
  let same_file x y =
    try
      let x = stat x in
      let y = stat y in
      x.st_dev = y.st_dev && x.st_ino = y.st_ino
    with
    | _ -> false
end

module Find = struct
  open Unix

  (* run [f] on files in [path] *)
  let folddir ~f ~init path =
    let dh = opendir path in
    protect ~f:(fun () ->
      let rec loop st =
	try
	  let st' = f st (readdir dh) in
	  loop st'
	with
	| End_of_file -> st
      in
      loop init)
      ~finally:(fun () -> closedir dh) ()
  ;;

  module Inodes = Set.Make(struct
    type t = int
    let compare : int -> int -> int = fun x y -> compare x y
  end)
  ;;
  
  type path = 
      { dir : string;
	base : string;
	path : string; (* dir / name *)
	stat : [ `Ok of stats | `Error of exn ];
	depth : int;
      }

  let path ~depth ~dir base =
    let path = match dir ^/ base with
      | "./." -> "."
      | s -> s
    in
    { dir = dir;
      base = base;
      path = path;
      depth = depth; 
      stat = try `Ok (stat path) with e -> `Error e;
    }
  ;;

  let kind path =
    match path.stat with
    | `Error _exn -> None
    | `Ok stat -> Some stat.st_kind
  ;;

  let is_dir path = kind path = Some S_DIR

  let inode path = 
    match path.stat with
    | `Ok stat -> Some stat.st_ino
    | `Error _ -> None
  ;;

  exception Prune

  let prune () = raise Prune

  let find ~f fnames =

    (* visited cache *)
    let visited = ref Inodes.empty in
    let if_not_visited_then path ~f = match inode path with
      | None -> ()
      | Some inode ->
	  if Inodes.mem inode !visited then ()
	  else begin
	    visited := Inodes.add inode !visited;
	    f path
	  end
    in

    let rec find_dir pth =
      try 
	f pth;
	let subdirs =
	  folddir pth.path ~init:[] ~f:(fun dirs -> function
	    | "." | ".." -> dirs
	    | name -> 
		let pth = path ~depth:(pth.depth + 1) ~dir:pth.path name in
		if try is_dir pth with _ -> false then pth::dirs
		else begin find_non_dir pth; dirs end)
	in
	List.iter (if_not_visited_then ~f:find_dir) subdirs
      with
      | Prune -> ()

    and find_non_dir path = try f path with Prune -> ()
    in

    List.iter (fun fname ->
      let path = 
	path ~depth: 0 ~dir:(Filename.dirname fname) (Filename.basename fname)
      in
      if is_dir path then find_dir path
      else find_non_dir path) fnames
  ;;
end

module Hashtbl = struct
  include Hashtbl

  let of_list size kvs =
    let tbl = Hashtbl.create size in
    List.iter (fun (k,v) ->
      Hashtbl.replace tbl k v) kvs;
    tbl

  let memoize tbl f k =
    try 
      Hashtbl.find tbl k 
    with
    | Not_found ->
        let v = f k in
        Hashtbl.replace tbl k v;
        v

  let find_default def tbl k = try find tbl k with Not_found -> def

  let multi_add tbl k v =
    let vs = v :: find_default [] tbl k in
    replace tbl k vs
end

module Hashset = struct
  (* poorman's hash set by hashtbl *)
  type 'a t = ('a, 'a) Hashtbl.t
  
  let create = Hashtbl.create
  let add set x = Hashtbl.replace set x x
  let remove = Hashtbl.remove
  let mem = Hashtbl.mem
  let find = Hashtbl.find
  let find_opt t k = try Some (Hashtbl.find t k) with Not_found -> None
  let iter f = Hashtbl.iter (fun v _ -> f v)
  let fold f = Hashtbl.fold (fun v _ st -> f v st)
  let elements = Hashtbl.length
  let clear = Hashtbl.clear
  
  let of_list size vs = 
    let set = create size in
    List.iter (add set) vs;
    set
  
  let to_list set = fold (fun x y -> x::y) set []
end

external (&) : ('a -> 'b) -> 'a -> 'b = "%apply"


