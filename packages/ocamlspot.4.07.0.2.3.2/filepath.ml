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

(* File path normalization *)

open Utils

module Filename = Copiedfilename

module type Filename = sig
  val current_dir_name : string
  val parent_dir_name : string
  val dir_sep : string
  val is_dir_sep : string -> int -> bool
  val is_relative : string -> bool
  val is_implicit : string -> bool
  val check_suffix : string -> string -> bool
  val temp_dir_name : string
  val quote : string -> string
  val basename : string -> string
  val dirname : string -> string

  val has_drive : string -> bool
  val drive_and_path : string -> string * string

  val normalize_drive : string -> string
  val is_network_drive : string -> bool
end

module Unix : Filename = struct
  include Filename.Unix
  let has_drive _ = false
  let drive_and_path s = "", s
  let normalize_drive s = s
  let is_network_drive _ = false
end

module Win32 : Filename = struct
  include Filename.Win32
    
  (* We think network drives too *)
  (* CR jfuruse: it returns true even for "///" *)
  let has_drive s =
    has_drive s 
    || match String.sub' s 0 2 with
      | "//" | "\\\\" -> true
      | _ -> false

  let drive_and_path s = 
    match drive_and_path s with
    | "", s ->
        begin match String.sub' s 0 2 with
        | ("//" | "\\\\" as p) -> p, String.sub s 2 (String.length s - 2)
        | _ -> "", s
        end
    | res -> res

  let normalize_drive s = String.replace_chars '/' '\\' (String.uppercase_ascii s)

  let is_network_drive = function
    | "//" | "\\\\" -> true
    | _ -> false
end

module Cygwin : Filename = struct
  include Filename.Cygwin
  let has_drive = Win32.has_drive
  let drive_and_path = Win32.drive_and_path
  let normalize_drive s = String.replace_chars '\\' '/' (String.lowercase_ascii s)
  let is_network_drive = Win32.is_network_drive
end

module Make(F : Filename) = struct
  class c = object
    method current = F.current_dir_name
    method parent = F.parent_dir_name
    method sep = F.dir_sep
    method is_relative = F.is_relative
    method is_absolute x = not (F.is_relative x)
    method check_suffix = F.check_suffix
    method dir_and_base s = F.dirname s, F.basename s
    method temp_dir = F.temp_dir_name
    method quote = F.quote
    method drive_and_path = F.drive_and_path
    method normalize_drive = F.normalize_drive
    method is_network_drive = F.is_network_drive
    method is_dir_sep = F.is_dir_sep
  end
end

module MakeUnix = Make(Unix)

type op = MakeUnix.c

let unix   = let module M = Make(Unix)   in new M.c
let win32  = let module M = Make(Win32)  in new M.c
let cygwin = let module M = Make(Cygwin) in new M.c

type os = 
  | Unix (** We love *)
  | Win32 (** We hate *)
  | Cygwin (** a failed effort of reconcillation *)

let of_os = function
  | Unix -> unix
  | Win32 -> win32
  | Cygwin -> cygwin

let os = match Sys.os_type with
  | "Unix" -> Unix
  | "Win32" -> Win32
  | "Cygwin" -> Cygwin
  | _ -> assert false

type t = { 
  os : os;
  op : op;
  drive : string option; (** Some "C:",  Some "\\\\" or Some "//" *)
  abs : bool;
  revs : string list; (** reversed directory components: a/b/c => ["a"; "b"; "c"] *)
  normalized : bool;
}

let of_string os s =
  let f = of_os os in
  let drive, p = f#drive_and_path s in
  let drive = if drive = "" then None else Some drive in
  let abs = match drive with
    | None -> f#is_absolute p 
    | Some d when f#is_network_drive d -> true
    | _ -> f#is_absolute p 
  in
  let rec splits st s =
    let d, b = f#dir_and_base s in
    if f#is_dir_sep d 0 && f#is_dir_sep b 0 then
      (* In Unix at least, it means [s] is ["/"] or ["////"]. *)
      st
    else
      if s = d then s :: st
      else splits (b::st) d
  in
  let revs = List.rev (splits [] p) in
  { os; 
    op = f;
    drive;
    abs;
    revs;
    normalized = false }
  
let normalize t =
  if t.normalized then t
  else
    let f = t.op in
    let drive = match t.drive with
      | None -> None
      | Some d -> Some (f#normalize_drive d)
    in
    (* xxx/./yyy => xxx/yyy
       xxx/a/../yyy => xxx/yyy
       /../../ => /../../
    *)
    let rec normalize_rev = function
      | [] -> []
      | x::xs when x = f#current -> normalize_rev xs
      | x::xs when x = f#parent ->
          let ys = normalize_rev xs in
          begin match ys with
          | [] when t.abs -> [] (* /.. => / *)
          | [] -> [x] (* .. => .. *)
          | z::_ when z = f#parent -> x::ys (* xxx/../.. => xxx/../.. *)
          | _::zs -> zs (* xxx/z/.. => xxx *)
          end
      | x::xs -> x :: normalize_rev xs
    in
    let revs = normalize_rev t.revs in
    { t with drive; revs; normalized = true }

let to_string t =
  let compos = List.rev t.revs in
  let concats = String.concat t.op#sep in
  match t.drive, t.abs, compos with
  | None, true, [] -> t.op#sep
  | None, true, _ -> concats ("" :: compos)
  | None, false, [] -> t.op#current
  | None, false, _ -> concats compos
  | Some d, true, _ when t.op#is_network_drive d -> d ^ concats compos
  | Some d, false, _ when t.op#is_network_drive d -> assert false
  | Some d, true, [] -> d ^ t.op#sep
  | Some d, true, _ -> d ^ concats ("" :: compos)
  | Some d, false, [] -> d ^ t.op#current
  | Some d, false, _ -> d ^ concats compos
      
let is_absolute t = t.abs
let is_relative t = not t.abs

let is_root t = t.abs && let t = normalize t in t.revs = []

let dirbase t = 
  let t = normalize t in
  match t.revs with
  | [] -> t, None
  | x::_ when x = t.op#parent -> invalid_arg "dirbase"
  | x::xs -> { t with revs = xs }, Some x

let (^/) x s = 
  let y = of_string x.os s in
  if is_absolute y then invalid_arg "(^/)"
  else normalize { x with revs = y.revs @ x.revs; normalized = false }

let concats x ss = List.fold_left (^/) x ss

let parent t = 
  let t = normalize t in
  match t.revs with
  | [] when t.abs -> t
  | [] -> { t with revs = [ t.op#parent ] }
  | x::_ when x = t.op#parent -> { t with revs = t.op#parent :: t.revs }
  | _::xs -> { t with revs = xs }

let wrap os f s = to_string (f (normalize (of_string os s)))

let is_prefix x y =
  if x.os = y.os && x.abs = y.abs then
    let rec is_prefix xs ys = match xs, ys with
      | [], ys -> Some ys
      | x::xs, y::ys when x = y -> is_prefix xs ys
      | _ -> None
    in
    is_prefix (List.rev x.revs) (List.rev y.revs)
  else None

let test () =
  let norm os s eq = 
    let res = wrap os (fun x -> x) s in
    if res <> eq then begin
      Format.eprintf "Filepath.test failed: %S => %S => %S@." s res eq;
      assert false
    end
  in
  List.iter (fun (os, s, eq) -> norm os s eq) 
    [ Unix, "/a/b/c", "/a/b/c";

      Unix, "a/b/c", "a/b/c";
      
      Unix, "//a/b/c", "/a/b/c";
      
      Unix, "///a/b/c", "/a/b/c";

      Unix, "/", "/";

      Unix, "//", "/";

      Unix, "///", "/";

      Unix, ".", ".";

      Unix, "./", ".";

      Unix, "/.", "/";

      Unix, "/a/./b/./c/", "/a/b/c";

      Unix, "/a/../b/../c/", "/c"; 

      Unix, "../../a/../b", "../../b";

      Unix, "..", "..";

      Unix, "/..", "/";

      Unix, "a/.", "a";

      Unix, "a//b/.", "a/b";

      Unix, "", "."; (* ??? *)
      
      Win32, "\\a\\b\\c", "\\a\\b\\c";

      Win32, "c:\\a\\b\\c", "C:\\a\\b\\c";

      Win32, "c:/a/b/c", "C:\\a\\b\\c";

      Win32, "c:a/b/c", "C:a\\b\\c";

      Win32, "c:", "C:.";

      Win32, "//a/b", "\\\\a\\b";
    ]
  
(*
let get_component : string -> string = Hashtbl.memoize (Hashtbl.create 1023) (fun x -> x)

let dotdot = get_component (parent_dir_name)

let hashcons_list = 
  let cache = Hashtbl.create 1023 in
  let rec f xs = Hashtbl.memoize cache (function
    | [] -> []
    | x::xs -> x :: f xs) xs
  in
  f
*)
