(** This module provides a functor to read DVI files *)

(** The type of colors in a DVI file *)
type color =
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | HSB of float * float * float
  | Gray of float

type info = { color : color }
(** The info type *)

(* A device can choose the way it want to see a text :
   - A string in some tex font at some position
   - A list of glyph of some type1 font each at his own position
*)
type env_info

type text = {
  tex_font : Fonts.t;
  tex_string : Int32.t list;
  tex_pos : float * float;
  tex_info : info;
  tex_env : env_info;
}

type text_type1 = {
  c_glyph : Int32.t;
  c_font : Fonts.type1;
  c_pos : float * float;
  c_info : info;
}

type command =
  | Fill_rect of info * float * float * float * float
      (** [Fill_rect info x y w h] should draw a rectangle at [(x,y)] of
      width [w] and height [h]. *)
  | Draw_text of text
  | Specials of info * string * float * float
      (** [Specials info s x y] should draw special [s], encoded as a string,
      at pos. [(x,y)]. [info] can contain additional information
      such as color. *)
  | Draw_text_type1 of text_type1
      (** Can appear only after a decomposition of text *)

type page = command list

val load_file : File.t -> page list
(** [load_file arg fn] loads the dvi document in file [fn], passes
      [arg] and the loaded document to {!Dev.new_document} and calls the
      drawing functions of {!Dev} as needed. At the end, the return
      value of the device is returned. The command list are
      in reverse order inside a page*)

val decompose_text : text -> command list

module Incremental : sig
  val load_page : Dvi.Incremental.t -> Dvi.page -> page
end

module Print : sig
  (* debug printing *)
  val command : Format.formatter -> command -> unit

  val page : Format.formatter -> page -> unit

  val dvi : Format.formatter -> page list -> unit
end
