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

(* $Id: oGif.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open OImages

let load_first name opts = OImages.make (Gif.load_first name opts)

let save_image name opts image =
  let img = image#image in
  match img with
  | Images.Index8 _bmp ->
      Gif.save_image name opts img
  | _ -> raise Wrong_image_class
