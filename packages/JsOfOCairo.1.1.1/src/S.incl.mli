(* Copyright 2017-2018 Vincent Jacques <vincent@vincent-jacques.net> *)

(* Operations not supported by HTML5 canvas.
We will probably not try to implement these functions.

type antialias = ANTIALIAS_DEFAULT | ANTIALIAS_NONE | ANTIALIAS_GRAY | ANTIALIAS_SUBPIXEL
val set_antialias: context -> antialias -> unit
val get_antialias: context -> antialias

val clip_reset: context -> unit

val set_tolerance: context -> float -> unit
val get_tolerance: context -> float

val copy_page: context -> unit
val show_page: context -> unit

val mask: context -> 'a Pattern.t -> unit
val mask_surface: context -> Surface.t -> x:float -> y:float -> unit

val clip_extents: context -> rectangle
val clip_rectangle_list: context -> rectangle list

val fill_extents: context -> rectangle
val in_fill: context -> x:float -> y:float -> bool

val stroke_extents: context -> rectangle
val in_stroke: context -> x:float -> y:float -> bool

module Pattern: sig
  type extend = NONE | REPEAT | REFLECT | PAD
  val set_extend: 'a t -> extend -> unit
  val get_extend: 'a t -> extend

  type filter = FAST | GOOD | BEST | NEAREST | BILINEAR
  val set_filter: 'a t -> filter -> unit
  val get_filter: 'a t -> filter
  val set_matrix: 'a t -> Matrix.t -> unit
  val get_matrix: 'a t -> Matrix.t

  I tried implementing surface patterns using createPattern with a new canvas element, but:
  - the repeat options don't match type extend
  - there is no support for transformations (only an experimental setTransform in Firefox)
  so I couldn't implement set_matrix and the x and y parameters of set_source_surface.

  val create_for_surface: Surface.t -> [`Surface] t
  val get_surface: [`Surface] t -> Surface.t
end

val set_source_surface: context -> Surface.t -> x:float -> y:float -> unit

*)

(* Other types and functions commented out below have not been analyzed yet.
They may or may not be implemented later. *)

type status = INVALID_RESTORE | INVALID_POP_GROUP | NO_CURRENT_POINT | INVALID_MATRIX | INVALID_STATUS | NULL_POINTER | INVALID_STRING | INVALID_PATH_DATA | READ_ERROR | WRITE_ERROR | SURFACE_FINISHED | SURFACE_TYPE_MISMATCH | PATTERN_TYPE_MISMATCH | INVALID_CONTENT | INVALID_FORMAT | INVALID_VISUAL | FILE_NOT_FOUND | INVALID_DASH | INVALID_DSC_COMMENT | INVALID_INDEX | CLIP_NOT_REPRESENTABLE | TEMP_FILE_ERROR | INVALID_STRIDE | FONT_TYPE_MISMATCH | USER_FONT_IMMUTABLE | USER_FONT_ERROR | NEGATIVE_COUNT | INVALID_CLUSTERS | INVALID_SLANT | INVALID_WEIGHT | INVALID_SIZE | USER_FONT_NOT_IMPLEMENTED | DEVICE_TYPE_MISMATCH | DEVICE_ERROR | INVALID_MESH_CONSTRUCTION | DEVICE_FINISHED | JBIG2_GLOBAL_MISSING

exception Error of status

val status_to_string: status -> string

exception Unavailable

type context

type matrix = {
  mutable xx: float;
  mutable yx: float;
  mutable xy: float;
  mutable yy: float;
  mutable x0: float;
  mutable y0: float;
}

module Matrix: sig
  type t = matrix

  val init_identity: unit -> t
  val init_translate: x:float -> y:float -> t
  val init_scale: x:float -> y:float -> t
  val init_rotate: angle:float -> t
  val translate: t -> x:float -> y:float -> unit
  val scale: t -> x:float -> y:float -> unit
  val rotate: t -> angle:float -> unit
  val invert: t -> unit
  val multiply: t -> t -> t
  val transform_distance: t -> dx:float -> dy:float -> float * float
  val transform_point: t -> x:float -> y:float -> float * float
end

type text_extents = {
  x_bearing: float;
  y_bearing: float;
  width: float;
  height: float;
  x_advance: float;
  y_advance: float;
}

(* module Glyph: sig
  type t = {
    index: int;
    x: float;
    y: float;
  }

  type cluster = {
    num_bytes: int;
    num_glyphs: int;
  }

  type cluster_flags = BACKWARD

  val extents: context -> t array -> text_extents
  val show: context -> t array -> unit
  val show_text: context -> string -> t array -> cluster array -> cluster_flags -> unit
end *)

(* type subpixel_order = SUBPIXEL_ORDER_DEFAULT | SUBPIXEL_ORDER_RGB | SUBPIXEL_ORDER_BGR | SUBPIXEL_ORDER_VRGB | SUBPIXEL_ORDER_VBGR *)

(* type hint_style = HINT_STYLE_DEFAULT | HINT_STYLE_NONE | HINT_STYLE_SLIGHT | HINT_STYLE_MEDIUM | HINT_STYLE_FULL *)

(* type hint_metrics = HINT_METRICS_DEFAULT | HINT_METRICS_OFF | HINT_METRICS_ON *)

(* module Font_options: sig
  type t

  val set: context -> t -> unit
  val get: context -> t
  val create: unit -> t
  val make: ?antialias:antialias -> ?subpixel_order:subpixel_order -> ?hint_style:hint_style -> ?hint_metrics:hint_metrics -> unit -> t
  val copy: t -> t
  val merge: t -> t -> unit
  val set_antialias: t -> antialias -> unit
  val get_antialias: t -> antialias
  val set_subpixel_order: t -> subpixel_order -> unit
  val get_subpixel_order: t -> subpixel_order
  val set_hint_style: t -> hint_style -> unit
  val get_hint_style: t -> hint_style
  val set_hint_metrics: t -> hint_metrics -> unit
  val get_hint_metrics: t -> hint_metrics
end *)

type slant = Upright | Italic | Oblique

type weight = Normal | Bold

(* type font_type = [ `Toy | `Ft | `Win32 | `Quartz | `User ] *)

(* module Font_face: sig
  type 'a t

  val set: context -> _ t -> unit
  val get: context -> font_type t
  val get_type: 'a t -> font_type
  val create: ?family:string -> slant -> weight -> [`Toy] t
  val get_family: [`Toy] t -> string
  val get_slant: [`Toy] t -> slant
  val get_weight: [`Toy] t -> weight
end *)

type font_extents = {
  ascent: float;
  descent: float;
  baseline: float;
  max_x_advance: float;
  max_y_advance: float;
}

(* module Scaled_font: sig
  type 'a t

  val set: context -> _ t -> unit
  val get: context -> 'a t
  val create: 'a Font_face.t -> Matrix.t -> Matrix.t -> Font_options.t -> 'a t
  val extents: _ t -> font_extents
  val text_extents: _ t -> string -> text_extents
  val glyph_extents: _ t -> Glyph.t array -> text_extents
  val text_to_glyphs: _ t -> x:float -> y:float -> string -> Glyph.t array * Glyph.cluster array * Glyph.cluster_flags
  val get_font_face: 'a t -> 'a Font_face.t
  val get_font_options: _ t -> Font_options.t
  val get_font_matrix: _ t -> Matrix.t
  val get_ctm: _ t -> Matrix.t
  val get_scale_matrix: _ t -> Matrix.t
  val get_type: 'a t -> font_type
end *)

val select_font_face: context -> ?slant:slant -> ?weight:weight -> string -> unit
val set_font_size: context -> float -> unit
(* val set_font_matrix: context -> Matrix.t -> unit *)
(* val get_font_matrix: context -> Matrix.t *)
val show_text: context -> string -> unit
val font_extents: context -> font_extents
val text_extents: context -> string -> text_extents

(* type rectangle = {
  x: float;
  y: float;
  w: float;
  h: float;
} *)

(* type content = COLOR | ALPHA | COLOR_ALPHA *)

(* module Surface: sig
  type t

  val create_similar: t -> content -> width:int -> height:int -> t
  val finish: t -> unit
  val flush: t -> unit
  val get_font_options: t -> Font_options.t
  val get_content: t -> content
  val mark_dirty: t -> unit
  val mark_dirty_rectangle: t -> x:int -> y:int -> w:int -> h:int -> unit
  val set_device_offset: t -> x:float -> y:float -> unit
  val get_device_offset: t -> float * float
  val set_fallback_resolution: t -> x:float -> y:float -> unit
  val get_fallback_resolution: t -> float * float

  type kind = [ `Image | `PDF | `PS | `XLib | `XCB | `GLITZ | `Quartz | `Win32 | `BEOS | `DirectFB | `SVG | `OS2 | `Win32_printing | `Quartz_image | `Recording ]

  val get_type: t -> kind
  val copy_page: t -> unit
  val show_page: t -> unit
  val has_show_text_glyphs: t -> bool
end *)

(* module Image: sig
  type format = | ARGB32 | RGB24 | A8 | A1

  val create: format -> width:int -> height:int -> Surface.t

  type data8 = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  type data32 = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array2.t

  val create_for_data8: data8 -> format -> ?stride:int -> int -> int -> Surface.t
  val create_for_data32: ?width:int -> ?height:int -> ?alpha:bool -> data32 -> Surface.t
  val get_data8: Surface.t -> data8
  val get_data32: Surface.t -> data32
  val get_format: Surface.t -> format
  val get_width: Surface.t -> int
  val get_height: Surface.t -> int
  val get_stride: Surface.t -> int
  val stride_for_width: format -> width:int -> int
  val output_ppm: out_channel -> ?width:int -> ?height:int -> data32 -> unit
end *)

(* module PDF: sig
  val create: fname:string -> width:float -> height:float -> Surface.t
  val create_for_stream: output:(string -> unit) -> width:float -> height:float -> Surface.t
  val set_size: Surface.t -> width:float -> height:float -> unit
end *)

(* module PNG: sig
  val create: string -> Surface.t
  val create_from_stream: input:(string -> int -> unit) -> Surface.t
  val write: Surface.t -> string -> unit
  val write_to_stream: Surface.t -> output:(string -> unit) -> unit
end *)

(* module PS: sig
  val create: fname:string -> width:float -> height:float -> Surface.t
  val create_for_stream: output:(string -> unit) -> width:float -> height:float -> Surface.t

  type level = LEVEL_2 | LEVEL_3

  val restrict_to_level: Surface.t -> level -> unit
  val get_levels: unit -> level list
  val level_to_string: level -> string
  val set_eps: Surface.t -> eps:bool -> unit
  val get_eps: Surface.t -> bool
  val set_size: Surface.t -> width:float -> height:float -> unit

  module Dsc: sig
    val begin_setup: Surface.t -> unit
    val begin_page_setup: Surface.t -> unit
    val comment: Surface.t -> string -> unit
  end
end *)

(* module SVG: sig
  val create: fname:string -> width:float -> height:float -> Surface.t
  val create_for_stream: output:(string -> unit) -> width:float -> height:float -> Surface.t

  type version = VERSION_1_1 | VERSION_1_2

  val restrict_to_version: Surface.t -> version -> unit
  val get_versions: unit -> version list
  val version_to_string: version -> string
end *)

(* module Recording: sig
  val create: ?extents:rectangle -> content -> Surface.t
  val ink_extents: Surface.t -> rectangle
end *)

module Pattern: sig
  type 'a t constraint 'a = [<`Solid | `Surface | `Gradient | `Linear | `Radial]
  type any = [`Solid | `Surface | `Gradient | `Linear | `Radial] t

  val add_color_stop_rgb: [> `Gradient] t -> ?ofs:float -> float -> float -> float -> unit
  val add_color_stop_rgba: [> `Gradient] t -> ?ofs:float -> float -> float -> float -> float -> unit
  val get_color_stop_count: [> `Gradient] t -> int
  val get_color_stop_rgba: [> `Gradient] t -> idx:int -> float * float * float * float * float
  val create_rgb: r:float -> g:float -> b:float -> [`Solid] t
  val create_rgba: r:float -> g:float -> b:float -> a:float -> [`Solid] t
  val get_rgba: [> `Solid] t -> float * float * float * float
  val create_linear: x0:float -> y0:float -> x1:float -> y1:float -> [`Linear | `Gradient] t
  val get_linear_points: [> `Linear | `Gradient] t -> float * float * float * float
  val create_radial: x0:float -> y0:float -> r0:float -> x1:float -> y1:float -> r1:float -> [`Radial | `Gradient] t
  val get_radial_circles: [> `Radial | `Gradient] t -> float * float * float * float * float * float
end

(* val create: Surface.t -> context *)
val save: context -> unit
val restore: context -> unit
(* val get_target: context -> Surface.t *)

(* module Group: sig
  val push: ?content:content -> context -> unit
  val pop: context -> Pattern.any
  val pop_to_source: context -> unit
  val get_target: context -> Surface.t
end *)

val set_source_rgb: context -> r:float -> g:float -> b:float -> unit
val set_source_rgba: context -> r:float -> g:float -> b:float -> a:float -> unit
val set_source: context -> 'a Pattern.t -> unit
val get_source: context -> Pattern.any

type line_cap = BUTT | ROUND | SQUARE
val set_line_cap: context -> line_cap -> unit
val get_line_cap: context -> line_cap

type line_join = JOIN_MITER | JOIN_ROUND | JOIN_BEVEL
val set_line_join: context -> line_join -> unit
val get_line_join: context -> line_join

val set_line_width: context -> float -> unit
val get_line_width: context -> float

val set_miter_limit: context -> float -> unit
val get_miter_limit: context -> float

val set_dash: context -> ?ofs:float -> float array -> unit
val get_dash: context -> float array * float

type operator = CLEAR | SOURCE | OVER | IN | OUT | ATOP | DEST | DEST_OVER | DEST_IN | DEST_OUT | DEST_ATOP | XOR | ADD | SATURATE
val set_operator: context -> operator -> unit
val get_operator: context -> operator

type fill_rule = WINDING | EVEN_ODD
val set_fill_rule: context -> fill_rule -> unit
val get_fill_rule: context -> fill_rule

val clip: context -> unit
val clip_preserve: context -> unit
val fill: context -> unit
val fill_preserve: context -> unit
val paint: ?alpha:float -> context -> unit
val stroke: context -> unit
val stroke_preserve: context -> unit

(* Arcs are translated to one or more CURVE_TO. Not supported by HTML5 and difficult to re-implement exactly. *)
(* type path_data =
  | MOVE_TO of float * float
  | LINE_TO of float * float
  | CURVE_TO of float * float * float * float * float * float
  | CLOSE_PATH *)

module Path: sig
  (* type t *)

  (* val copy: context -> t *)
  (* val copy_flat: context -> t *)
  (* val append: context -> t -> unit *)
  val get_current_point: context -> float * float
  val clear: context -> unit
  (* val sub: context -> unit *)
  val close: context -> unit
  (* val glyph: context -> Glyph.t array -> unit *)
  (* val text: context -> string -> unit *)
  (* val extents: context -> rectangle *)
  (* val fold: t -> ('a -> path_data -> 'a) -> 'a -> 'a *)
  (* val to_array: t -> path_data array *)
  (* val of_array: path_data array -> t *)
end

val arc: context -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
val arc_negative: context -> x:float -> y:float -> r:float -> a1:float -> a2:float -> unit
val curve_to: context -> x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val line_to: context -> x:float -> y:float -> unit
val move_to: context -> x:float -> y:float -> unit
val rectangle: context -> x:float -> y:float -> w:float -> h:float -> unit
val rel_curve_to: context -> x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
val rel_line_to: context -> x:float -> y:float -> unit
val rel_move_to: context -> x:float -> y:float -> unit
val translate: context -> x:float -> y:float -> unit
val scale: context -> x:float -> y:float -> unit
val rotate: context -> angle:float -> unit
val transform: context -> Matrix.t -> unit
val set_matrix: context -> Matrix.t -> unit
val get_matrix: context -> Matrix.t
val identity_matrix: context -> unit
val user_to_device: context -> x:float -> y:float -> float * float
val user_to_device_distance: context -> x:float -> y:float -> float * float
val device_to_user: context -> x:float -> y:float -> float * float
val device_to_user_distance: context -> x:float -> y:float -> float * float
