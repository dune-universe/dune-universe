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

(* $Id: ftlow.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

(**
  Almost all of the functions are just interpretation of functions of
  the freetype library. See the documents of the freetype library.
*)

type library

val init : unit -> library
val close : library -> unit

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

val new_face : library -> string -> int -> face
val face_info : face -> face_info
val done_face : face -> unit

val get_num_glyphs : face -> int

val set_char_size : face -> int -> int -> int -> int -> unit

val set_pixel_sizes : face -> int -> int -> unit

type charmap = { platform_id : int; encoding_id : int; }
val get_charmaps : face -> charmap list
val set_charmap : face -> charmap -> unit

val get_char_index : face -> int -> int

type render_mode =
   | Render_Normal (** default *)
   | Render_Mono

type load_flag =
   | Load_no_scale
   | Load_no_hinting

val load_glyph : face -> int -> load_flag list -> int * int

val load_char : face -> int -> load_flag list -> int * int

val render_glyph_of_face : face -> render_mode -> unit

val render_glyph : face -> int -> load_flag list -> render_mode -> int * int

val render_char : face -> int -> load_flag list -> render_mode -> int * int

val set_transform : face -> (int * int * int * int) -> (int * int) -> unit

type bitmap_info = {
    bitmap_left : int;
    bitmap_top : int;
    bitmap_width : int;
    bitmap_height : int;
  }

val get_bitmap_info : face -> bitmap_info

val read_bitmap : face -> int -> int -> int

(** glyph metrics *)
type bbox = {
    xmin : int; (** 26.6 *)
    ymin : int; (** 26.6 *)
    xmax : int; (** 26.6 *)
    ymax : int; (** 26.6 *)
  }

type bearing_advance = {
    bearingx : int; (** 26.6 *)
    bearingy : int; (** 26.6 *)
    advance : int; (** 26.6 *)
  }

type glyph_metrics = {
    gm_width : int (** 26.6 *);
    gm_height : int (** 26.6 *);
    gm_hori : bearing_advance;
    gm_vert : bearing_advance;
  }

val get_glyph_metrics : face -> glyph_metrics

(** size metrics *)
type size_metrics = {
    x_ppem : int;
    y_ppem : int;
    x_scale : int; (** 16.16 *)
    y_scale : int; (** 16.16 *)
  }

val get_size_metrics : face -> size_metrics

(** outline info *)
type outline_tag = On_point | Off_point_conic | Off_point_cubic

type outline_contents = {
    n_contours : int;
    n_points : int;
    points : (int * int) array;
    tags : outline_tag array;
    contours : int array;
  }

val get_outline_contents : face -> outline_contents
