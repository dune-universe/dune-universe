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

(* $Id: imgstat.ml,v 1.2 2004/09/21 18:15:42 weis Exp $ *)

open Images

let () = Arg.parse []
 (fun s ->
    try
      let f, h = file_format s in
      Printf.printf "%s %s %dx%d\n"
        s (extension f) h.header_width h.header_height
    with
    | _ -> Printf.printf "%s ???\n" s)
 "imgstat files"
