(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004                                                *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oXvthumb.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

open OImages

let load name =
  let info, img = Xvthumb.load name in
  info, new index8_wrapper img

let save name info img =
  match img#image with
  | Images.Index8 bmp -> Xvthumb.save name info bmp
  | _ -> raise Wrong_image_class

let create img = new index8_wrapper (Xvthumb.create img#image)
