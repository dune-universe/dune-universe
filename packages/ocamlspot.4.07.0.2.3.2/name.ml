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

type t = string

let create name = function
  | -1 -> Printf.sprintf "%s__G" name
  | n -> Printf.sprintf "%s__%d" name n
;;

let parse s =
  try
    let pos = String.rindex s '_' in
    if pos = 0 then raise Not_found;
    if s.[pos-1] <> '_' then raise Not_found;
    let n = String.sub s 0 (pos-1) in
    let id =
      match String.sub s (pos+1) (String.length s - pos - 1) with
      | "G" -> -1
      | s -> int_of_string s
    in
    n, id
  with
  | _ -> raise Not_found
