(***********************************************************************)
(*                                                                     *)
(*                           CamlImages                                *)
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

(* $Id: test.ml,v 1.32.2.1 2010/05/13 13:14:47 furuse Exp $ *)

open Images
open Format

let capabilities () =
  let supported b = if b then "supported" else "not supported" in
  printf "*******************************************************@.";
  printf "Camlimages library capabilities currently available@.";
  printf "bmp\t: %s@." (supported Camlimages.lib_bmp);
  printf "ppm\t: %s@." (supported Camlimages.lib_ppm);
  printf "gif\t: %s@." (supported Camlimages.lib_gif);
  printf "jpeg\t: %s@." (supported Camlimages.lib_jpeg);
  printf "tiff\t: %s@." (supported Camlimages.lib_tiff);
  printf "png\t: %s@." (supported Camlimages.lib_png);
  printf "xpm\t: %s@." (supported Camlimages.lib_xpm);
  printf "xv thumbnails\t: %s@." (supported Camlimages.lib_xvthumb);
  printf "postscript\t: %s@." (supported Camlimages.lib_ps);
  printf "freetype\t: %s@." (supported Camlimages.lib_freetype);
  printf "*******************************************************@."

let show_image img x y =
  let img = 
    match img with
    | Rgba32 img -> Rgb24 (Rgb24.of_rgba32 img)
    | _ -> img
  in
  let gr_img = Graphics.make_image (Graphic_image.array_of_image img) in
  Graphics.draw_image gr_img x y

module FtDraw = Fttext.Make(Rgb24)

let images = [
  "apbm.pbm"; "apgm.pgm"; "appm.ppm";
  "pbm.pbm"; "pgm.pgm"; "ppm.ppm";
  "jpg.jpg"; "png.png"; "png-alpha.png"; "bmp.bmp"; "tif.tif";
  "xpm.xpm"; "eps.eps"; "gif.gif"; "mmm.anim.gif";
]

let treat_image name0 =
  let name = "images/" ^ name0 in
  prerr_endline (name ^ "...");
  try
    let format, header = Images.file_format name in
    prerr_endline
      (Printf.sprintf "%s: %s format, %dx%d"
         name (extension format) header.header_width header.header_height);
    match format with
    | Gif ->
        let sequence = Gif.load name [] in
        Gif.save ("out-" ^ name0) [] sequence
    | _ ->
        let img = Images.load name [] in
        Images.save ("out-" ^ name0) (Some format) [] img;
  with
  | Wrong_file_type -> prerr_endline "file format detection failed"
  | Failure s -> prerr_endline s

let main () =
  capabilities ();
  try List.iter treat_image images
  with
  | Exit -> exit 0
  | End_of_file -> exit 0
  | Sys.Break -> exit 2

let () = main ()
