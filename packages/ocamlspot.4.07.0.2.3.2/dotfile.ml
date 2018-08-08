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

(* module for .ocamlspot file 

   build_dir=dirname

      Work around for build systems which create object files to exotic places.
      If .ocamlspot is placed in a directory $DIR,
      then .cmt* files of source files under $DIR ex. $DIR/subdir/source.ml
      is searched in $DIR/dirname/subdir/subdir/.
*)

open Utils

let rec find ~dir:absdir ~name =
  if not & Unix.is_dir absdir then assert false (* failwithf "%s must be a directory" absdir*); 
  let path = absdir ^/ name in
  if Sys.file_exists path then Some (0, "", absdir, path)
  else if absdir = "/" then None
  else 
    match find ~dir:(Filename.dirname absdir) ~name with
    | None -> None
    | Some (n, postfix, absdir', path) -> 
        Some (n+1, postfix ^/ Filename.basename absdir , absdir', path)
;;

type t = {
  dir_level : int;
  build_dir : string option;
  module_prefix : string option;
}

(* very strict .ini file style format *)
let split_by_equal s =
  try
    let pos = String.index s '=' in
    let key = String.sub s 0 pos in
    let value = String.sub s (pos+1) (String.length s - pos - 1) in
    key, Some value
  with
  | Not_found -> s, None
;;

let load_raw s =
  let ic = open_in s in
  let tbl = Hashtbl.create 20 in
  let rec load () =
    let line = input_line ic in
    let key, value = split_by_equal line in
    if Hashtbl.mem tbl key then
      failwithf "key %S already bound" key
    else
      Hashtbl.replace tbl key value;
    load ()
  in
  try load () with End_of_file ->
    close_in ic;
    tbl
;;

let load lev s =
  let build_dir = ref None in
  let module_prefix = ref None in
  let set name ref v =
    match !ref with
    | Some _ -> failwithf "key %s is defined twice" name
    | None -> ref := Some v
  in
  let ic = open_in s in
  let rec load () =
    let line = input_line ic in
    let key, value = split_by_equal line in
    match key, value with
    | "build_dir", Some s -> 
        set "build_dir" build_dir s;
        load ()
    | "module_prefix", Some s -> 
        set "module_prefix" module_prefix s;
        load ()
    | ("build_dir" | "module_prefix"), None -> failwithf "key %S must have a value" key
    | key, _ -> failwithf "unknown key %S" key
  in
  try load () with End_of_file ->
    close_in ic;
    { dir_level = lev
    ; build_dir = !build_dir
    ; module_prefix = !module_prefix }
;;

let find_and_load absdir =
  match find ~dir:absdir ~name:".ocamlspot" with
  | None -> None
  | Some (n, postfix, dir, path) -> 
      Format.eprintf ".ocamlspot loaded from %s@." path;
      Some (postfix, dir, load n path)
;;
