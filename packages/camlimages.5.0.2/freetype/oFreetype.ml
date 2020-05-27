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

(* $Id: oFreetype.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Freetype

let library = ref None (* We do not want more than 1 engine, don't we ? *)

let get_library () =
  match !library with
  | None ->
      let e = init () in
      library := Some e;
      e
  | Some e -> e

class face file id =
  let library = get_library () in
  let face, _info = new_face library file id in

  object

  method library = library
  method face = face

  method set_char_size = set_char_size face
  method set_pixel_sizes = set_pixel_sizes face

  method num_glyphs = get_num_glyphs face

  method charmaps = get_charmaps face
  method set_charmap = set_charmap face
  method char_index = get_char_index face

  method load_glyph = load_glyph face
  method load_char = load_char face
  method render_glyph_of_face = render_glyph_of_face face
  method render_glyph = render_glyph face
  method render_char = render_char face

  method set_transform = set_transform face

  method glyph_metrics = get_glyph_metrics face

  method size_metrics = get_size_metrics face

  method outline_contents = get_outline_contents face

  method size string = Fttext.size face string
  method size_of_glyphs string = Fttext.size_of_glyphs face string
end

let draw_gen render_mode renderf rot func face px py string =
  let face = face#face in
  let matrix = matrix_rotate rot in
  let curx = ref (0.0) and cury = ref (0.0) in

  for i = 0 to Array.length string - 1 do
    set_transform face matrix {ft_x= !curx; ft_y= !cury};
    let advx, advy = renderf face string.(i) [] render_mode in
    let binfo = get_bitmap_info face in

    for y = 0 to binfo.bitmap_height - 1 do
      for x = 0 to binfo.bitmap_width - 1 do
  	let z = read_bitmap face x y in
  	let level =
  	  if z < 0 then 0 else
  	  if z > 255 then 255 else z
  	in
	let px = px + binfo.bitmap_left + x
	and py = py - (binfo.bitmap_top - binfo.bitmap_height + y)
	in
	func px py level
      done;
    done;
    curx := !curx +. advx;
    cury := !cury +. advy
  done

let draw_rotated_gen render_mode face (func : 'a -> int -> 'a)
    (image : 'a OImages.map) px py rot renderf string =

  (*let putpixel px py level =
    try
      let orgcolor = image#get px py in
      image#set px py (func orgcolor level);
    with
      Out_of_image -> ()
  in*)

  let face = face#face in
  let matrix = matrix_rotate rot in
  let curx = ref (0.0) and cury = ref (0.0) in

  for i = 0 to Array.length string - 1 do
    set_transform face matrix {ft_x= !curx; ft_y= !cury};
    let advx, advy = renderf face string.(i) [] render_mode in
    let binfo = get_bitmap_info face in

    for y = 0 to binfo.bitmap_height - 1 do
      for x = 0 to binfo.bitmap_width - 1 do
  	let z = read_bitmap face x y in
  	let level =
  	  if z < 0 then 0 else
  	  if z > 255 then 255 else z
  	in
  	try
	  let px = px + binfo.bitmap_left + x
	  and py = py - (binfo.bitmap_top - binfo.bitmap_height + y)
	  in
  	  let orgcolor = image#get px py in
  	  image#set px py (func orgcolor level);
  	with
  	  Out_of_image -> ()
      done;
    done;
    curx := !curx +. advx;
    cury := !cury +. advy
  done

let draw_rotated_text face func image x y rot string =
  draw_rotated_gen Render_Normal face func image x y rot render_char string

let draw_rotated_glyphs face func image x y rot string =
  draw_rotated_gen Render_Normal face func image x y rot render_glyph string

let draw_text face func image x y string =
  draw_rotated_text face func image x y 0.0 string

let draw_glyphs face func image x y string =
  draw_rotated_glyphs face func image x y 0.0 string

(* mono *)
let draw_mono_rotated_text face func image x y rot string =
  draw_rotated_gen Render_Mono face func image x y rot render_char string

let draw_mono_rotated_glyphs face func image x y rot string =
  draw_rotated_gen Render_Mono face func image x y rot render_glyph string

let draw_mono_text face func image x y string =
  draw_mono_rotated_text face func image x y 0.0 string

let draw_mono_glyphs face func image x y string =
  draw_mono_rotated_glyphs face func image x y 0.0 string

(* Vector based *)

let vector_text turn_y func face px py rot string =
  Fttext.vector_gen load_char turn_y rot func face#face px py string

let vector_glyphs turn_y func face px py rot string =
  Fttext.vector_gen load_glyph turn_y rot func face#face px py string
