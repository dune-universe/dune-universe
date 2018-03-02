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

(* $Id: seq.ml,v 1.3 2004/09/21 18:15:46 weis Exp $ *)

open Images

let load_sequence_as_pixmaps ~window file =
  let seq = load_sequence file [] in
  let seq = unoptimize_sequence seq in
  List.map
    (fun frame ->
       Ximage.pixmap_of_image window None
         frame.frame_image, frame.frame_delay)
    seq.seq_frames
