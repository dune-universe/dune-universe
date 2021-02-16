open Dvi_util

(** Load fonts and extract information *)

type type1 = private {
  glyphs_tag : int;
  (* unique tag *)
  glyphs_ft : Mlpost_ft.t;
  (* the file, pfb or pfa, which define the glyphs *)
  glyphs_enc : int -> int;
  (* the conversion of the charactersx between tex and the font *)
  slant : float option;
  extend : float option;
  glyphs_ratio_cm : float;
}

type vf = private {
  vf_design_size : float;
  vf_font_map : Dvi_util.font_def Dvi_util.Int32Map.t;
  (* Font on which the virtual font is defined *)
  vf_chars : Dvi.command list Int32H.t;
      (* The dvi command which define each character*)
}

type glyphs = Type1 of type1 | VirtualFont of vf

type t
(** the type of a loaded font *)

val load_font : font_def -> float -> t
(** [load_font def f] loads font [def] scaled by [f] *)

val metric : t -> Tfm.t
(** Obtain the font metric *)

val tex_name : t -> string
(** get the name of the font as used by TeX *)

val ratio_cm : t -> float
(** The font ratio, in cm *)

val glyphs : t -> glyphs

val char_width : t -> int -> float

val char_height : t -> int -> float

val char_depth : t -> int -> float
(** get information about the [i]th char of the font *)

val char_dims : t -> int -> float * float * float
(** width, height, depth of the [i]th char *)

val scale : t -> float -> float
(** [scale t f] scale the given float [f] by [ratio_cm t]  *)

val design_size : t -> float
(** the design size of the font *)
