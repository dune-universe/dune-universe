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

(* $Id: freetype.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

type t (** type for Freetype library *)

val init : unit -> t
    (** [init ()] initializes the Freetype library.
       The returned library is required to load fonts. *)

type face (** Type for face *)

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

val new_face : t -> string -> int -> face * face_info
    (** [new_face library fontfile n] loads [n]-th font stored
       in the font file [fontfile], and returns its face and
       face information. *)

val get_num_glyphs : face -> int
    (** Returns number of glyphs stored in the face.
       Equivalent to face_info.num_glyphs *)

val set_char_size : face -> float -> float -> int -> int -> unit
    (** [set_char_size face charw charh resh resv] sets the character
       size of [face]. [charw] and [charh] are the points of the characters
       in width and height. [resh] and [resv] are the horizontal and
       vertical resolution (in dpi) *)

val set_pixel_sizes : face -> int -> int -> unit
    (** [set_pixel_sizes face pixw pixh] also sets the character size
       of [face]. [pixw] and [pixh] are standard width and height of
       characters in pixels. *)

type charmap = { platform_id: int; encoding_id: int; }

type char_index

val int_of_char_index : char_index -> int
val char_index_of_int : int -> char_index

val get_charmaps : face -> charmap list
val set_charmap : face -> charmap -> unit
val get_char_index : face -> int -> char_index

type render_mode = Render_Normal | Render_Mono
type load_flag = Load_no_scale | Load_no_hinting
(** if you give [], freetype loads glyphs with scaling and hinting *)

val load_glyph : face -> char_index -> load_flag list -> float * float
val load_char : face -> int -> load_flag list -> float * float
val render_glyph_of_face : face -> render_mode -> unit
val render_glyph :
  face -> char_index -> load_flag list -> render_mode -> float * float
val render_char :
  face -> int -> load_flag list -> render_mode -> float * float

(** matrix and vector *)
type matrix = { ft_xx : float; ft_xy : float; ft_yx : float; ft_yy : float; }
type vector = { ft_x : float; ft_y : float; }

val set_transform : face -> matrix -> vector -> unit
val matrix_rotate : float -> matrix

(** bitmap ops *)
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
    xmin: float;
    ymin: float;
    xmax: float;
    ymax: float;
  }

type bearing_advance = {
    bearingx: float;
    bearingy: float;
    advance: float;
  }

type glyph_metrics = {
    gm_width: float;
    gm_height: float;
    gm_hori: bearing_advance;
    gm_vert: bearing_advance;
  }

val get_glyph_metrics : face -> glyph_metrics

(** size metrics *)

type size_metrics = {
    x_ppem: int;
    y_ppem: int;
    x_scale: float;
    y_scale: float;
  }

val get_size_metrics : face -> size_metrics

(** outline info *)
type outline_tag = On_point | Off_point_conic | Off_point_cubic

type outline_contents = {
    n_contours : int;
    n_points : int;
    points : (float * float) array;
    tags : outline_tag array;
    contours : int array;
  }

val get_outline_contents : face -> outline_contents
