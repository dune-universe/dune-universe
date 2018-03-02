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

(* $Id: resize.ml,v 1.11 2004/09/21 18:15:48 weis Exp $ *)

open Images

let file = ref ""
let scale = ref 1.0

let () = Arg.parse [
  "-scale", Arg.Float (fun sc -> scale := sc), "scale"; ]
  (fun s -> file := s)
  "resize -scale ? file"

let file = !file
let scale = !scale
let outfile = "out" ^ file

let () = 
  Bitmap.maximum_live := 15000000; (* 60MB *)
  Bitmap.maximum_block_size := !Bitmap.maximum_live / 16;
  let r = Gc.get () in
  r.Gc.max_overhead <- 30;
  Gc.set r;

  let fmt, _ = Images.file_format file in
  let img = OImages.load file [] in

  let img = OImages.rgb24 img in

  let nw = truncate (float img#width *. scale)
  and nh = truncate (float img#height *. scale) in
  let newimage = img#resize None nw nh in
  img#destroy;
  newimage#save outfile (Some fmt) [Save_Quality 95]
