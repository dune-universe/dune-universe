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

(* $Id: tiff.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

val check_header : string -> Images.header
  (** Checks the file header *)

val load : string -> Images.load_option list -> Images.t
  (** Loads a tiff image. *)

val save : string -> Images.save_option list -> Images.t -> unit
  (** Save a full-color image in tiff format file.
     Raises [Invalid_argument] if the image is not a full-color image. *)

(** Scanline based I/O functions *)

type colormodel = RGB | CMYK | WHITEBLACK | BLACKWHITE

type in_handle

val open_in : string -> int * int * float * colormodel * in_handle
val read_scanline : in_handle -> bytes -> int -> unit
val close_in : in_handle -> unit

type out_handle

val open_out : string -> int -> int -> float -> out_handle
val write_scanline : out_handle -> bytes -> int -> unit
val close_out : out_handle -> unit



