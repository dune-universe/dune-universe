open Re

module Gen = Regexp_gen

type 'a t = ('a, Re.re) Gen.t

module REX : sig
  val regexp : ?flags:Re_pcre.flag list -> string -> re
  val exec : rex:re -> ?pos:int -> string -> groups
  val get_substring_ofs : groups -> int -> int * int
  val get_opt_substrings : groups -> full_match:bool -> string option array
  val replace : re -> templ:string -> ?pos:int -> string -> string
  val replace_first : re -> templ:string -> ?pos:int -> string -> string
  val substitute : re -> (groups -> string) -> ?pos:int -> string -> string
  val substitute_first : re -> (groups -> string) -> ?pos:int -> string -> string
  val split : rex:re -> string -> string list
end = struct
  let regexp = Re_pcre.regexp
  let exec = Re_pcre.exec
  let get_substring_ofs = Re_pcre.get_substring_ofs
  let get_opt_substrings substrs ~full_match:b =
    assert (b=true);
    Array.mapi (fun i -> function
      | (-1,-1) -> None
      | _ -> Some (Re_pcre.get_substring substrs i)) (Re.Group.all_offset substrs)
  let replace rex ~templ ?pos = Re.replace ?pos ~all:true rex ~f:(fun _ -> templ)
  let replace_first rex ~templ ?pos = Re.replace ?pos ~all:false rex ~f:(fun _ -> templ)
  let substitute rex f ?pos s = Re.replace ?pos ~all:true rex ~f s
  let substitute_first rex f ?pos s = Re.replace ?pos ~all:false rex ~f s
  let split = Re_pcre.split
end

module Literal = struct

  module OrakudaRegexpInternal = struct

    class group = Gen.group
  
    let create string ~flags binder = Gen.create string (REX.regexp ~flags string) binder
    
    let make_group_obj rex s substrs =
      let subject_start, subject_end = REX.get_substring_ofs substrs 0 in (* may raise an exception *)
      let left = String.sub s 0 subject_start in
      let right = String.sub s subject_end (String.length s - subject_end) in
      let groups = REX.get_opt_substrings substrs ~full_match:true in
      Gen.make_group_obj rex left groups right 

    let replace rex ~templ ?pos = REX.replace rex.Gen.re ~templ ?pos
    let replace_first rex ~templ ?pos = REX.replace_first rex.Gen.re ~templ ?pos
    let substitute_substrings f rex ?pos s = REX.substitute ?pos rex.Gen.re (fun substrs -> f (make_group_obj rex s substrs)) s
    let substitute_substrings_first f rex ?pos s = REX.substitute_first ?pos rex.Gen.re (fun substrs -> f (make_group_obj rex s substrs)) s
  end
end

open Literal.OrakudaRegexpInternal

let regexp t = t.Gen.re

let exec_exn rex (* ?iflags ?flags *) ?pos (* ?callout *) s =
  let substrs = REX.exec ~rex:rex.Gen.re (* ?iflags ?flags *) ?pos (* ?callout *) s in
  make_group_obj rex s substrs
;;

let exec rex (* ?iflags ?flags *) ?pos (* ?callout *) s =
  try
    Some (exec_exn rex (* ?iflags ?flags *) ?pos (* ?callout *) s)
  with
  | Not_found -> None

let replace = replace
let replace_first = replace_first
let substitute_substrings = substitute_substrings
let substitute_substrings_first = substitute_substrings_first

(* split *)
let split rex = REX.split ~rex:rex.Gen.re

module Infix = struct
  let (=~) s (* ?iflags ?flags *) ?pos (* ?callout *) rex = 
    exec rex (* ?iflags ?flags *) ?pos (* ?callout *) s
  ;;

  let case s = `Error s
  
  let (==>) r f = function
    | `Ok v -> `Ok v
    | `Error s -> 
        match s =~ r with
        | Some v -> `Ok (f v)
        | None -> `Error s
  
  let default f = function
    | `Ok v -> v
    | `Error _ -> f ()
end

let build_case rex f s =
  match exec rex s with
  | Some v -> Some (f v)
  | None -> None

let build_cases ?(default= fun () -> raise Not_found) rex_fs s = 
  let rec with_aux = function
    | [] -> default ()
    | rex_f::rex_fs ->
	match rex_f s with
	| Some v -> v
	| None -> with_aux rex_fs
  in
  with_aux rex_fs
;;

