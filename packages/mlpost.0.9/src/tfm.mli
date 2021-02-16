(** A low level interface for TeX Font Metric (Tfm) files *)

(** The structure of the types follows very closely the one of Tfm files. See
   documentation for this file type to get more detailed information about the
   record fields. *)

type file_hdr = {
  lf : int;  (** length of the entire file, in words *)
  lh : int;  (** length of the header data, in words *)
  bc : int;  (** smallest character code in the font *)
  ec : int;  (** largest character code in the font  *)
  nw : int;  (** number of words in the width table  *)
  nh : int;  (** number of words in the height table *)
  nd : int;  (** number of words in the depth table  *)
  ni : int;  (** number of words in the italic correction table *)
  nl : int;  (** number of words in the lig/kern table *)
  nk : int;  (** number of words in the kern table   *)
  ne : int;  (** number of words in the extensible character table *)
  np : int;  (** number of font parameter words      *)
}

type fix_word = float

type header = {
  checksum : int32;  (** font checksum *)
  design_size : fix_word;  (** The font design size *)
  coding_scheme : string option;
  identifier : string option;
  seven_bit_safe_flag : int option;
  face : int option;
}

type char_info_word = {
  width_index : int;
  height_index : int;
  depth_index : int;
  italic_index : int;
  tag : int;
  info_remainder : int;
}
(** information about a char *)

type lig_kern_command = {
  skip_byte : int;
  next_char : int;
  op_byte : int;
  kern_remainder : int;
}
(** a kerning command *)

type extensible_recipe = { top : int; mid : int; bot : int; rep : int }
(** information about extensible characters *)

type body = {
  header : header;
  char_info : char_info_word array;
  width : fix_word array;
  height : fix_word array;
  depth : fix_word array;
  italic : fix_word array;
  lig_kern : lig_kern_command array;
  kern : fix_word array;
  exten : extensible_recipe array;
  param : fix_word array;
}
(** the body of a Tfm file *)

type t = { file_hdr : file_hdr; body : body }
(** A tfm file *)

val read_file : string -> t
(** read a tfm file *)

val design_size : t -> float
(** accessor for the [design_size] of the font *)
