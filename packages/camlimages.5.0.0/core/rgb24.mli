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

(* $Id: rgb24.mli,v 1.6 2009/07/04 03:39:28 furuse Exp $ *)

(* RGB 24 bit depth image format *)

type elt = Color.rgb

type rawimage

(** Image type *)
type t = {
  width : int;
  height : int;
  rawimage : rawimage;
  mutable infos : Info.info list;
}


val resize : (float -> unit) option -> t -> int -> int -> t
(* [resize progress t w h] creates a resized image of [t] with size [w]x[h].
   You can specify a progress meter function [progress], which takes
   a float from 0 to 1.0. *)

val to_rgba32 : t -> Rgba32.t
val of_rgba32 : Rgba32.t -> t
(** Image format conversion functions between Rgba32.t images *)

(** Generic functions *)
(* Please read the comments of IMAGE in genimage.mli *)

val create_with : int -> int -> Info.info list -> bytes -> t
val create_with_scanlines : int -> int -> Info.info list -> bytes array -> t
val create : int -> int -> t
val make : int -> int -> elt -> t
val destroy : t -> unit
val get : t -> int -> int -> elt
val set : t -> int -> int -> elt -> unit
val unsafe_get : t -> int -> int -> elt
val unsafe_set : t -> int -> int -> elt -> unit
val get_strip : t -> int -> int -> int -> bytes
val set_strip : t -> int -> int -> int -> bytes -> unit
val get_scanline : t -> int -> bytes
val get_scanline_ptr : t -> (int -> (bytes * int) * int) option
val set_scanline : t -> int -> bytes -> unit
val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
val map : (elt -> elt -> elt) ->
  t -> int -> int -> t -> int -> int -> int -> int -> unit

val blocks : t -> int * int
val dump_block : t -> int -> int -> Bitmap.Block.t
val copy : t -> t
val sub : t -> int -> int -> int -> int -> t
val dump : t -> bytes
val unsafe_access : t -> int -> int -> bytes * int
val rawimage : t -> rawimage
