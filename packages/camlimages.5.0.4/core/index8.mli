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

(* $Id: index8.mli,v 1.4 2009/07/04 03:39:28 furuse Exp $*)

(** Indexed 8 bit depth image format *)

type elt = int

type rawimage

(* The image type *)
type t = {
  width : int;
  height : int;
  rawimage : rawimage;
  mutable infos : Info.info list;
  mutable colormap : Color.rgb Color.map;
  mutable transparent : int;
 }

val to_rgb24 : ?failsafe: Color.rgb -> t -> Rgb24.t
val to_rgba32 : ?failsafe: Color.rgba -> t -> Rgba32.t
(* [to_rgb? ~failsafe t]: Image format conversion functions to Rgb24.t
   and Rgba32.t images. If the color for some pixel value is not defined,
   [failsafe] color is used as default. *)

(** Generic functions *)
(** Please read the comments of IMAGEINDEXED in genimage.mli *)

val dump : t -> bytes
val unsafe_access : t -> int -> int -> bytes * int
val get_strip : t -> int -> int -> int -> bytes
val set_strip : t -> int -> int -> int -> bytes -> unit
val get_scanline : t -> int -> bytes
val set_scanline : t -> int -> bytes -> unit
val unsafe_get : t -> int -> int -> elt
val unsafe_set : t -> int -> int -> elt -> unit
val get : t -> int -> int -> elt
val set : t -> int -> int -> elt -> unit
val unsafe_get_color : t -> int -> int -> Color.rgb
val get_color : t -> int -> int -> Color.rgb
val unsafe_get_rgb : t -> int -> int -> Color.rgb
val get_rgb : t -> int -> int -> Color.rgb
val destroy : t -> unit
val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
val blocks : t -> int * int
val dump_block : t -> int -> int -> Bitmap.Block.t
val map : (elt -> elt -> elt) ->
  t -> int -> int -> t -> int -> int -> int -> int -> unit
val create_with : int -> int ->
  Info.info list -> Color.rgb Color.map -> int -> bytes -> t
val create_with_scanlines : int -> int ->
  Info.info list -> Color.rgb Color.map -> int -> bytes array -> t
val create : int -> int -> t
val make : int -> int -> elt -> t
val copy : t -> t
val sub : t -> int -> int -> int -> int -> t
val rawimage : t -> rawimage
