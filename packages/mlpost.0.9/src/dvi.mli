(** Low-level DVI interface *)

type preamble = {
  pre_version : int;
  pre_num : int32;
  pre_den : int32;
  pre_mag : int32;
  pre_text : string;
}
(** The DVI preamble *)

type postamble = {
  last_page : int32;
  post_num : int32;
  post_den : int32;
  post_mag : int32;
  post_height : int32;
  post_width : int32;
  post_stack : int;
  post_pages : int;
}
(** The DVI postamble *)

type postpostamble = { postamble_pointer : int32; post_post_version : int }
(** The DVI postpostamble *)

(** The type of commands. All coordinates in this type are relative to the
    current state of the DVI document. *)
type command =
  | SetChar of int32
  | SetRule of int32 * int32
  | PutChar of int32
  | PutRule of int32 * int32
  | Push
  | Pop
  | Right of int32
  | Wdefault
  | W of int32
  | Xdefault
  | X of int32
  | Down of int32
  | Ydefault
  | Y of int32
  | Zdefault
  | Z of int32
  | FontNum of int32
  | Special of string

type page = {
  counters : int32 array;
  previous : int32;
  commands : command list;
}
(** A page is a list of commands *)

type fontmap = Dvi_util.font_def Dvi_util.Int32Map.t

type t = {
  preamble : preamble;
  pages : page list;
  postamble : postamble;
  postpostamble : postpostamble;
  font_map : fontmap;
}
(** A document is a list of pages, plus a preamble, postamble,
   postpostamble and font map *)

val get_conv : t -> float
(** a few accessor functions *)

val fontmap : t -> fontmap

val commands : page -> command list

val pages : t -> page list

val read_file : string -> t

val get_height_cm : t -> float

val get_width_cm : t -> float

(** Vf files *)

(* Vf type *)

type preamble_vf = {
  pre_vf_version : int;
  pre_vf_text : string;
  pre_vf_cs : int32;
  pre_vf_ds : float;
}

type char_desc = {
  char_code : int32;
  char_tfm : int32;
  char_commands : command list;
}

type vf = {
  vf_preamble : preamble_vf;
  vf_font_map : fontmap;
  vf_chars_desc : char_desc list;
}

val print_vf : Format.formatter -> vf -> unit

val read_vf_file : string -> vf

module Incremental : sig
  (** Useful to read a DVI file page per page *)

  type t
  (** The type that stores information regarding the DVI file *)

  (*   val mk_t : in_channel -> t *)

  val mk_t : in_channel -> t * page list

  val next_pages : t -> page list
  (** read all available pages *)

  val get_conv : t -> float

  val font_map : t -> fontmap
end

module Print : sig
  val page : Format.formatter -> page -> unit

  val pages : Format.formatter -> page list -> unit

  val page_verb : Format.formatter -> page -> unit

  val pages_verb : Format.formatter -> page list -> unit
end
