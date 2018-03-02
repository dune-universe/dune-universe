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

(* $Id: fttext.ml,v 1.2 2008/06/16 22:35:42 furuse Exp $ *)

open Images
open Freetype

type 'a drawer = 'a -> int -> 'a

let func_darken_only org level =
  let level = 255 - level in
  { r = if org.r > level then level else org.r;
    g = if org.g > level then level else org.g;
    b = if org.b > level then level else org.b }

let func_red_only _org _level = { r = 255; g = 0; b = 0 }

let unicode_of_latin s =
  Array.init (String.length s) @@ fun i -> Char.code s.[i]

let unicode_of_euc_japan s = Jis_unicode.encode s

let draw_gen render_mode renderf rot func face px py string =
  let matrix = matrix_rotate rot in
  let curx = ref (0.0) and cury = ref (0.0) in

  for i = 0 to Array.length string - 1 do
    set_transform face matrix {ft_x = !curx; ft_y = !cury};
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
(*
            in
	    and py = py + (binfo.bitmap_top + binfo.bitmap_height - y)
*)
	  in
	  func px py level
  	with
  	  Out_of_image -> ()
      done;
    done;
    curx := !curx +. advx;
    cury := !cury +. advy;
  done

let draw_rotated_text = draw_gen Render_Normal render_char
let draw_rotated_glyphs = draw_gen Render_Normal render_glyph
let draw_text = draw_rotated_text 0.0
let draw_glyphs = draw_rotated_glyphs 0.0

let draw_mono_rotated_text = draw_gen Render_Mono render_char
let draw_mono_rotated_glyphs = draw_gen Render_Mono render_glyph
let draw_mono_text = draw_mono_rotated_text 0.0
let draw_mono_glyphs = draw_mono_rotated_glyphs 0.0


module type T = sig
  type t
  type elt

  val create : int -> int -> t  
  val destroy : t -> unit
  val get : t -> int -> int -> elt
  val set : t -> int -> int -> elt -> unit
  val unsafe_get : t -> int -> int -> elt
  val unsafe_set : t -> int -> int -> elt -> unit
end

module Make(T : T) = struct

  let putpixel f bitmap = fun px py level ->
    try
      let orgcolor = T.get bitmap px py in
      T.set bitmap px py (f orgcolor level) 
    with
      Out_of_image -> ()

  let draw_rotated_text face func bitmap px py rot string =
    draw_rotated_text rot (putpixel func bitmap) face px py string

  let draw_rotated_glyphs face func bitmap px py rot string =
    draw_rotated_glyphs rot (putpixel func bitmap) face px py string

  let draw_text face func bitmap px py string =
    draw_text (putpixel func bitmap) face px py string

  let draw_glyphs face func bitmap px py string =
    draw_glyphs (putpixel func bitmap) face px py string

  let draw_mono_rotated_text face func bitmap px py rot string =
    draw_mono_rotated_text rot (putpixel func bitmap) face px py string

  let draw_mono_rotated_glyphs face func bitmap px py rot string =
    draw_mono_rotated_glyphs rot (putpixel func bitmap) face px py string

  let draw_mono_text face func bitmap px py string =
    draw_mono_text (putpixel func bitmap) face px py string

  let draw_mono_glyphs face func bitmap px py string =
    draw_mono_glyphs (putpixel func bitmap) face px py string

end

let size_gen face loadf string =
  let curx = ref 0.0
  and leftmost = ref None
  and rightmost = ref None
  and upmost = ref None
  and downmost = ref None
  in
  for i = 0 to Array.length string - 1 do
    let _advx, _advy = loadf face string.(i) [] in
    let metrics = get_glyph_metrics face in
    let left = metrics.gm_hori.bearingx +. !curx
    and right = metrics.gm_hori.bearingx +. metrics.gm_width +. !curx
    and up = metrics.gm_hori.bearingy
    and down = metrics.gm_hori.bearingy -. metrics.gm_height
    in
    begin match !leftmost with
    | None -> leftmost := Some left
    | Some x when x > left -> leftmost := Some left 
    | _ -> () end;
    begin match !rightmost with
    | None -> rightmost := Some right 
    | Some x when x < right -> rightmost := Some right 
    | _ -> () end;
    begin match !upmost with
    | None   -> upmost := Some up 
    | Some x when x < up -> upmost := Some up 
    | _ -> () end;
    begin match !downmost with
    | None   -> downmost := Some down 
    | Some x when x > down -> downmost := Some down 
    | _ -> () end;
    curx := !curx +. metrics.gm_hori.advance
  done;
  match !leftmost, !downmost, !rightmost, !upmost with
    Some l, Some d, Some r, Some u -> l,d,r,u
  | _ -> assert false

let size face string = size_gen face load_char string
let size_of_glyphs face string = size_gen face load_glyph string

let vector_gen loadf turn_y rot func face px py string =
  let matrix = matrix_rotate rot in
  let matrix = 
    if turn_y then 
      { matrix with ft_xy = -. matrix.ft_xy;
	ft_yy = -. matrix.ft_yy; }
    else matrix
  in 
  let curx = ref px and cury = ref py in

  for i = 0 to Array.length string - 1 do
    set_transform face matrix {ft_x = !curx; ft_y = !cury};
    let advx, advy = loadf face string.(i) [] in
    func (get_outline_contents face);
    curx := !curx +. advx;
    cury := !cury +. advy
  done

let vector_text turn_y func face px py rot string =
  vector_gen load_char turn_y rot func face px py string
  
let vector_glyphs turn_y func face px py rot string =
  vector_gen load_glyph turn_y rot func face px py string
