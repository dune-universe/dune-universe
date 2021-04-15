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

(* Screen shot. *)
open Images;;

let save_gr_image fname x y nw nh =
  let screen_w = Graphics.size_x () and screen_h = Graphics.size_y () in
  let img = Graphic_image.get_image x y screen_w screen_h in
  let cimg =
    if nw = screen_w && nh = screen_h then img else
    Rgb24.resize None img nw nh in
  Images.save fname None [] (Rgb24 cimg)
;;

let thumbnails =
 Options.flag false "-thumbnails"
 "  create thumbnails for your slides\
 \n\t and write them into cachedir,\
 \n\t (the default is to skip thumbnails construction).";;

let thumbnails_size_w = ref 24;;
let thumbnails_size_h = ref 32;;
let set_thumbnails_size s =
  let g = Ageometry.parse s in
  thumbnails_size_w := g.Ageometry.width;
  thumbnails_size_h := g.Ageometry.height;;

Options.add
 "-thumbnails-size"
 (Arg.String set_thumbnails_size)
 "<geom>: set the thumbnails geometry size to <geom>,\
 \n\t (the default geometry is \"24x32\").";;

let save n =
  if !thumbnails then
    let jpegfname =
      Filename.concat (Userfile.get_advi_cache_dir ())
        (Printf.sprintf "shot%d.jpg" n) in
    let bbfname =
      Filename.concat (Userfile.get_advi_cache_dir ())
        (Printf.sprintf "shot%d.bb" n) in
    save_gr_image jpegfname 0 0 !thumbnails_size_w !thumbnails_size_h;
    let bboc = open_out bbfname in
    let s =
      Printf.sprintf "%%%%BoundingBox: 0 0 %d %d\n"
        !thumbnails_size_w !thumbnails_size_h in
    output_string bboc s;
    flush bboc;
    close_out bboc;;




