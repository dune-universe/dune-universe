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

(* $Id: oTiff.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

let load name opts = OImages.make (Tiff.load name opts)

let save name opts image =
  let rgb24 = OImages.rgb24 image in
  Tiff.save name opts rgb24#image
