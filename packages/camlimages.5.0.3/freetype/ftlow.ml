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

(* $Id: ftlow.ml,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

(* The lower interface *)

type library

type face

type face_info = {
    num_faces : int;
    num_glyphs : int;
    family_name : string;
    style_name : string;
    has_horizontal : bool;
    has_vertical : bool;
    has_kerning : bool;
    is_scalable : bool;
    is_sfnt : bool;
    is_fixed_width : bool;
    has_fixed_sizes : bool;
    has_fast_glyphs : bool;
    has_glyph_names : bool;
    has_multiple_masters : bool;
  }

type charmap = { platform_id : int; encoding_id : int; }

type render_mode =
   | Render_Normal (* default *)
   | Render_Mono

type load_flag =
   | Load_no_scale
   | Load_no_hinting

type flags = int

type bitmap_info = {
    bitmap_left : int;
    bitmap_top : int;
    bitmap_width : int;
    bitmap_height : int;
  }

(* glyph metrics *)
type bbox = {
    xmin : int; (* 26.6 *)
    ymin : int; (* 26.6 *)
    xmax : int; (* 26.6 *)
    ymax : int;  (* 26.6 *)
  }

type bearing_advance = {
    bearingx : int; (* 26.6 *)
    bearingy : int; (* 26.6 *)
    advance : int; (* 26.6 *)
  }

type glyph_metrics = {
    gm_width : int (* 26.6 *);
    gm_height : int (* 26.6 *);
    gm_hori : bearing_advance;
    gm_vert : bearing_advance;
  }

(* size metrics *)
type size_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : int; (* 16.16 *)
    y_scale : int; (* 16.16 *)
  }

(* outline info *)
type outline_tag = On_point | Off_point_conic | Off_point_cubic

type outline_contents = {
    n_contours : int;
    n_points : int;
    points : (int * int) array;
    tags : outline_tag array;
    contours : int array;
  }

module C = struct
  external init : unit -> library = "init_FreeType"
  external close : library -> unit = "done_FreeType"
  external new_face : library -> string -> int -> face = "new_Face"
  external face_info : face -> face_info = "face_info"
  external done_face : face -> unit = "done_Face"
  external get_num_glyphs : face -> int = "get_num_glyphs"
  external set_char_size :
    face -> int -> int -> int -> int -> unit = "set_Char_Size"
  external set_pixel_sizes : face -> int -> int -> unit = "set_Pixel_Sizes"
  external get_charmaps : face -> charmap list = "get_CharMaps"
  external set_charmap : face -> charmap -> unit = "set_CharMap"
  external get_char_index : face -> int -> int = "get_Char_Index"
  external load_glyph_raw : face -> int -> flags -> int * int = "load_Glyph"
  external load_char_raw : face -> int -> flags -> int * int = "load_Char"
  external render_glyph_of_face :
    face -> render_mode -> unit = "render_Glyph_of_Face"
  external render_char_raw :
    face -> int -> flags -> render_mode -> int * int = "render_Char"
  external set_transform :
    face -> (int * int * int * int) -> (int * int) -> unit = "set_Transform"
  external get_bitmap_info : face -> bitmap_info = "get_Bitmap_Info"
  external read_bitmap : face -> int -> int -> int = "read_Bitmap"
  external get_glyph_metrics : face -> glyph_metrics = "get_Glyph_Metrics"
  external get_size_metrics : face -> size_metrics = "get_Size_Metrics"
  external get_outline_contents :
    face -> outline_contents = "get_Outline_Contents"
end

include C

let encode_flags flags =
  let int_of_flag = function
    | Load_no_scale -> 1
    | Load_no_hinting -> 2 in
  List.fold_left (fun v f -> v lor int_of_flag f) 0 flags

let load_glyph face code flags = load_glyph_raw face code (encode_flags flags)

let load_char face code flags = load_char_raw face code (encode_flags flags)

let render_glyph face idx flags render_mode =
  let adv = load_glyph face idx flags in
  render_glyph_of_face face render_mode;
  adv

let render_char face code flags render_mode =
  render_char_raw face code (encode_flags flags) render_mode

