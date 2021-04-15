(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Jun Furuse, Didier Rémy and Pierre Weis.                           *)
(*  Contributions by Roberto Di Cosmo, Didier Le Botlan,               *)
(*  Xavier Leroy, and Alan Schmitt.                                    *)
(*                                                                     *)
(*  Based on Mldvi by Alexandre Miquel.                                *)
(***********************************************************************)

(* $Id$ *)

(* Saving screen shots of the actual Active-DVI display.
   Pierre Weis.                                                        *)

open Images;;

type x = int and y = int and w = int and h = int;;

let get_page_image_file_suffix, set_page_image_file_suffix =
 let file_suffix = ref "png" in
 (fun () -> !file_suffix),
 (fun s -> file_suffix := s);;

Options.add
  "-page-image-file-suffix"
  (Arg.String set_page_image_file_suffix)
  (Printf.sprintf
     "<string>  set to <string> the suffix of page \
     \n\t image files saved by pressing ^X-^S during the presentation,\
     \n\t (the default is %S)." (get_page_image_file_suffix ()));;

let get_page_image_file_prefix, set_page_image_file_prefix =
 let file_prefix = ref "./shot" in
 (fun () -> !file_prefix),
 (fun s -> file_prefix := s);;

Options.add
  "-page-image-file-prefix"
  (Arg.String set_page_image_file_prefix)
  (Printf.sprintf
     "<string>  set to <string> the prefix of page \
     \n\t image files saved by pressing ^X-^S during the presentation,\
     \n\t (the default is %S)." (get_page_image_file_prefix ()));;

(* File numbering counter. *)
let get_page_image_file_number, incr_page_image_file_number,
    set_page_image_file_number =
  let cntr = ref 0 in
  (fun () -> !cntr),
  (fun () -> incr cntr),
  (fun i -> cntr := i);;

Options.add
  "-page-image-file-first-number"
  (Arg.Int set_page_image_file_number)
  (Printf.sprintf
     "<int>  set to <int> the starting number of page \
     \n\t image files saved by pressing ^X-^S during the presentation,\
     \n\t (the default is %d)." (get_page_image_file_number ()));;

let new_fname () =
  let fname =
    Printf.sprintf "%s%d.%s"
      (get_page_image_file_prefix ()) (get_page_image_file_number ())
      (get_page_image_file_suffix ()) in
  incr_page_image_file_number ();
  fname;;

let output_page_area_image_file fname x y w h =
  GraphicsY11.anti_synchronize ();
  let img = Graphic_image.get_image x y w h in
  Images.save fname None [] (Rgb24 img);
  Misc.debug_endline (Printf.sprintf "image %s has been written." fname);;

let save_page_area_image_file fname x y w h =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  let x =
   if x < 0 then 0 else
   if x > screen_w then screen_w - 1 else
   x in
  let y =
   if y < 0 then 0 else
   if y > screen_h then screen_h - 1 else
   y in
  let h =
   if h < 0 then 0 else
   if h + y > screen_h then screen_h - y else
   h in
  let w =
   if w < 0 then 0 else
   if w + x > screen_w then screen_w - x else
   w in
  output_page_area_image_file fname x y w h;;

let save_page_area_image x y w h =
  let fname = new_fname () in
  save_page_area_image_file fname x y w h;;

let save_page_image_file fname =
  let w = Graphics.size_x () and h = Graphics.size_y () in
  output_page_area_image_file fname 0 0 w h;;

let save_page_image () =
  let fname = new_fname () in
  save_page_image_file fname;;
