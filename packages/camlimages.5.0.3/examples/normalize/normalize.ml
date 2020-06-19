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

(* $Id: normalize.ml,v 1.7 2004/09/24 10:55:10 weis Exp $ *)

let files = ref []


let () = Arg.parse []
  (fun s -> files := s :: !files)
  "normalize src dst"


let src, dst =
  match List.rev !files with
  | [src; dst] -> src, dst
  | _ -> assert false


let src = OImages.rgb24 (OImages.load src [])


let _ =
  (* Make monochrome *)
  let hist = Colorhist.create () in
  for x = 0 to src#width - 1 do
    for y = 0 to src#height - 1 do
      Colorhist.store_sample hist (src#get x y)
    done
  done;
  prerr_endline "histogram done";
  let normalizer = Colorhist.normalize 0.9 hist in
  prerr_endline "normalizer done";
  for x = 0 to src#width - 1 do
    for y = 0 to src#height - 1 do
      let rgb = src#get x y in
      let new_rgb = normalizer rgb in
      src#set x y new_rgb;
    done
  done


let () = src#save dst None []
