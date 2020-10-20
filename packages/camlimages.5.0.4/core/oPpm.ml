(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999, 2004                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: oPpm.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

open OImages

let load name opts = OImages.make (Ppm.load name opts)

let save name opts image =
  match image#image_class with
  | ClassRgb24 | ClassIndex8 -> Ppm.save name opts image#image
  | _ -> raise Wrong_image_class
