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

(* $Id: converter.ml,v 1.11 2004/09/21 18:15:41 weis Exp $ *)

open Images
open OImages

let () =
  let files = ref [] in
  Arg.parse [] (fun s -> files := s :: !files) "converter src dst";
  let src, dst =
    match List.rev !files with
    | [src; dst] -> src, dst
    | _ -> invalid_arg "you need two arguments" in
  
  let src = OImages.load src [] in
  
  let saver src = src#save dst None [] in
  try saver src with
  | Wrong_image_type ->
    (* try to use another color model *)
    let src' =
      match OImages.tag src with
      | Rgb24 _img -> invalid_arg "Sorry! No color reduction is implemented"
      | Index8 img -> img#to_rgb24#coerce
      | Index16 img -> img#to_rgb24#coerce
      | _ -> invalid_arg "not supported" in
    saver src'
