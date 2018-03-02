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

let draw_string =
  if Camlimages.lib_freetype then begin
    (* Freetype library initialization *)
    let library = Freetype.init () in
    let src_dir = (try Sys.getenv "srcdir" with Not_found -> ".") in
    let src_file = Filename.concat src_dir "micap.ttf" in
    let face, _face_info = Freetype.new_face library src_file 0 in
    Freetype.set_char_size face 18.0 18.0 72 72;

    fun str x y ->
      let str = Fttext.unicode_of_latin str in
      let x1,y1,x2,y2 = Fttext.size face str in
      let w = truncate (x2 -. x1) + 2
      and h = truncate (y2 -. y1) + 2 in
      let tmpbitmap = Rgb24.create w h in
      for x = 0 to w - 1 do
        for y = 0 to h - 1 do
          Rgb24.unsafe_set tmpbitmap x y { r = 255; g = 255; b = 255 }
        done
      done;
      FtDraw.draw_text face Fttext.func_darken_only tmpbitmap
          (- (truncate x1)) (truncate y2) str;
      show_image (Rgb24 tmpbitmap) x (y - h)
  end else fun _ _ _ -> ()

let go_on () =
 prerr_endline "Press return to proceed, s: save a screenshot, q: quit";
 let s = input_char stdin in
 (* save screen shot *)
 if s = 's' then begin
   prerr_endline "Saving screenshot";
   let gr_img =
     Graphic_image.get_image 0 0 (Graphics.size_x ()) (Graphics.size_y ()) in
   Images.save "screen.bmp" (Some Bmp) [] (Rgb24 gr_img);
   prerr_endline "done"
 end;
 s <> 'q'

let images_default = [
  "apbm.pbm"; "apgm.pgm"; "appm.ppm";
  "pbm.pbm"; "pgm.pgm"; "ppm.ppm";
  "jpg.jpg"; "png.png"; "png-alpha.png"; "bmp.bmp"; "tif.tif";
  "xpm.xpm"; "eps.eps"; "gif.gif"; "mmm.anim.gif";
]

let images =
  let images = ref [] in
  Arg.parse [] (fun x -> images := x :: !images) "test images";
  if !images <> []
  then List.rev !images
  else 
    let src_dir = (try Sys.getenv "srcdir" with Not_found -> ".") in
    let images_src_dir = Filename.concat src_dir "images" in
    List.map (fun x -> Filename.concat images_src_dir x) images_default

open Gif

let treat_image name =
  prerr_endline name;
  try
    prerr_endline "Analysing header...";
    let format, header = Images.file_format name in
    prerr_endline
      (Printf.sprintf "%s: %s format, %dx%d"
         name (extension format) header.header_width header.header_height);
    match format with
    | Gif ->
        prerr_endline ("Loading " ^ name ^ "...");
        let sequence = Gif.load name [] in
        prerr_endline "Loaded";
        let w = sequence.screen_width
        and h = sequence.screen_height in
        let w' = Graphics.size_x () - w
        and h' = Graphics.size_y () - h in
        let x = if w' > 0 then Random.int w' else 0
        and y = if h' > 0 then Random.int h' else 0 in
        draw_string name x y;
        List.iter (fun frame ->
          let put_x = x + frame.frame_left
          and put_y = y + frame.frame_top in
          show_image (Index8 frame.frame_bitmap) put_x put_y;
          (* if not (go_on ()) then raise Exit *) )
          sequence.frames;
        begin
          try
            Gif.save "out.image" [] sequence;
            prerr_endline "Saved";
          with
          | _ -> prerr_endline "Save failed"
        end;
        if not (go_on ()) then raise Exit
    | _ ->
        prerr_endline ("Loading " ^ name ^ "...");
        let img = Images.load name [] in
        prerr_endline "Loaded";
        let w, h = Images.size img in
        let w' = Graphics.size_x () - w
        and h' = Graphics.size_y () - h in
        let x = if w' > 0 then Random.int w' else 0
        and y = if h' > 0 then Random.int h' else 0 in
        show_image img x y;
        draw_string name x y;
        begin
          try
            Images.save "out.image" (Some format) [] img;
            prerr_endline "Saved";
          with
          | _ -> prerr_endline "Save failed"
        end;
        if not (go_on ()) then raise Exit
  with
  | Wrong_file_type -> prerr_endline "file format detection failed"
  | Failure s -> prerr_endline s

let main () =
  capabilities ();
  Graphics.open_graph "";
  try List.iter treat_image images
  with
  | Exit -> exit 0
  | End_of_file -> exit 0
  | Sys.Break -> exit 2

let () = main ()
