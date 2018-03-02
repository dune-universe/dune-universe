(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            François Pessaux, projet Cristal, INRIA Rocquencourt     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: gif.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

(** High level interfaces *)
type gif_extension = 
   | GifComment of string list
   | GifGraphics of string list
   | GifPlaintext of string list
   | GifApplication of string list
   | GifOtherExt of int * string list

type gif_frame = {
  frame_left : int;
  frame_top : int;
  frame_bitmap : Index8.t;
  mutable frame_extensions : gif_extension list;
  frame_delay : int;
}

type gif_sequence = {
  screen_width : int;
  screen_height : int;
  screen_colormap : Color.rgb Color.map;
  frames : gif_frame list;
  loops : int;
}

val check_header : string -> Images.header
  (** Checks the file header *)

val load : string -> Images.load_option list -> gif_sequence
  (** Loads a gif image sequence *)

val load_sequence : string -> Images.load_option list -> Images.sequence
  (** Loads a gif image sequence, but to more general type *)

val load_first : string -> Images.load_option list -> Images.t
  (** Loads the first frame of a gif image sequence. *)

val save : string -> Images.save_option list -> gif_sequence -> unit
  (** Saves a gif image sequence *)

val save_image : string -> Images.save_option list -> Images.t -> unit
  (** Saves an image as a gif file with only one frame *)

(*** Below they are all low level interfaces *)

type in_channel
type out_channel

type screen_info = {
  s_width : int;
  s_height : int;
  s_color_resolution : int;
  s_back_ground_color : int;
  s_colormap : Color.rgb array;
}

type record_type =
  | Undefined
  | Screen_desc
  | Image_desc
  | Extension
  | Terminate

type gif_desc = {
  desc_left : int;
  desc_top : int;
  desc_width : int;
  desc_height : int;
  desc_interlace : bool;
  desc_colormap : Color.rgb array;
}

val dGifOpenFileName : string -> screen_info * in_channel
val dGifCloseFile : in_channel -> unit
val dGifGetRecordType : in_channel -> record_type
val dGifGetImageDesc : in_channel -> gif_desc
val dGifGetLine : in_channel -> bytes
val dGifGetExtension : in_channel -> int * string list

val eGifOpenFileName : string -> out_channel
val eGifCloseFile : out_channel -> unit
val eGifPutScreenDesc : out_channel ->screen_info -> unit
val eGifPutImageDesc : out_channel -> gif_desc -> unit
val eGifPutLine : out_channel -> bytes -> unit
val eGifPutExtension : out_channel -> int * string list -> unit
