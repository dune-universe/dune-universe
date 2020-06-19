(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: iconcap.ml,v 1.16 2005/02/27 05:48:26 furuse Exp $ *)

open Livmisc

(* iconcap file parser *)

let table = Hashtbl.create 107

let load f =
  let ic = open_in f in
  try
    while true do
      let str = input_line ic in
      if str <> "" && str.[0] <> '#' then
      try
        let pos = String.index str ';' in
        let typ = remove_space (String.sub str 0 pos) in
        let icon =
          remove_space
            (String.sub str (pos + 1) (String.length str - pos - 1)) in
        let tokens =
          Mstring.split_str (function '/' -> true | _ -> false) typ in
        match tokens with
        | [maj; min] ->
            (* prerr_endline (Printf.sprintf "%s/%s => (%s)" maj min icon); *)
            Hashtbl.add table (maj, min) icon
        | _ -> assert false
      with
      | _ -> failwith (str ^ ": parse error")
    done;
    raise Exit
  with
  | End_of_file -> close_in ic

let () = try
 load
   (Pathfind.find
      [ "."; "~/.liv"; "/usr/lib/liv"; "/usr/local/lib/liv"; ]
      "iconcap")
with
| _ -> prerr_endline "no iconcap..."
