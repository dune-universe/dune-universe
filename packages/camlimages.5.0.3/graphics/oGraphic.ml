(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oGraphic.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

open Images
open OImages

let draw image x y =
  try Graphic_image.draw_image image#image x y with
  | Wrong_image_type -> raise Wrong_image_class

let get x y w h = new rgb24_wrapper (Graphic_image.get_image x y w h)
