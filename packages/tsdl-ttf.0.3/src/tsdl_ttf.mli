module Ttf : sig

(** SDL2_ttf bindings for use with Tsdl

    The UCS-2 Unicode bindings have been omitted; UCS-2 is obsolete,
    and the current implementation of SDL2_ttf converts such strings
    to UTF-8 before using them anyway.

    {{:https://www.libsdl.org/projects/SDL_ttf/docs/index.html}SDL2_ttf API}
*)

type 'a result = 'a Tsdl.Sdl.result

val init : unit -> unit result
val quit : unit -> unit
val was_init : unit -> bool

type font
val close_font : font -> unit

val open_font : string -> int -> font result
val open_font_index : string -> int -> int64 -> font result
val open_font_rw : Tsdl.Sdl.rw_ops -> int -> int -> font result
val open_font_index_rw : Tsdl.Sdl.rw_ops -> int -> int -> int64 -> font result

module Style : sig
  type t
  val ( + ) : t -> t -> t
  val test : t -> t -> bool
  val eq : t -> t -> bool
  val normal : t
  val bold : t
  val italic : t
  val underline : t
  val strikethrough : t
end
val get_font_style : font -> Style.t
val set_font_style : font -> Style.t -> unit

val get_font_outline : font -> int
val set_font_outline : font -> int -> unit

module Hinting : sig
  type t = Normal | Light | Mono | None
end
val get_font_hinting : font -> Hinting.t
val set_font_hinting : font -> Hinting.t -> unit

val get_font_kerning_size : font -> int -> int -> int

val font_height : font -> int
val font_ascent : font -> int
val font_descent : font -> int
val font_line_skip : font -> int
val get_font_kerning : font -> bool
val set_font_kerning : font -> bool -> unit
val font_faces : font -> int64
val font_face_is_fixed_width : font -> int
val font_face_family_name : font -> string
val font_face_style_name : font -> string
val glyph_is_provided : font -> int -> bool

module GlyphMetrics : sig
  type t = { min_x:int; max_x:int; min_y:int; max_y:int; advance:int }
end
val glyph_metrics : font -> int -> GlyphMetrics.t result

val size_text : font -> string -> (int*int) result
val size_utf8 : font -> string -> (int*int) result

val render_text_solid :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_utf8_solid :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_glyph_solid :
  font -> int -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_text_shaded :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_utf8_shaded :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_glyph_shaded :
  font -> int -> Tsdl.Sdl.color -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_text_blended :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_utf8_blended :
  font -> string -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result
val render_text_blended_wrapped :
  font -> string -> Tsdl.Sdl.color -> int32 -> Tsdl.Sdl.surface result
val render_utf8_blended_wrapped :
  font -> string -> Tsdl.Sdl.color -> int32 -> Tsdl.Sdl.surface result
val render_glyph_blended :
  font -> int -> Tsdl.Sdl.color -> Tsdl.Sdl.surface result

end
