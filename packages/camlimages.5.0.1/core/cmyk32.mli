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

(* $Id: cmyk32.mli,v 1.4 2009/07/04 03:39:28 furuse Exp $*)
 
(** CMYK 32 bit depth image format *)

type elt = Color.cmyk

type rawimage

(** The image type *)
type t = {
  width : int; 
  height : int;
  rawimage : rawimage;
  mutable infos : Info.info list;
 }

(** Generic functions

    Please read the comments of IMAGE in genimage.mli 
*)

(* CR jfuruse: 
include Image_intf.RAWIMAGE with type t = t and type elt = elt *)

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
val destroy : t -> unit
val blit : t -> int -> int -> t -> int -> int -> int -> int -> unit
val map : (elt -> elt -> elt) ->
  t -> int -> int -> t -> int -> int -> int -> int -> unit
val blocks : t -> int * int
val dump_block : t -> int -> int -> Bitmap.Block.t
val create_with : int -> int -> Info.info list -> bytes -> t
val create_with_scanlines : int -> int -> Info.info list -> bytes array -> t
val create : int -> int -> t
val make : int -> int -> elt -> t
val copy : t -> t
val sub : t -> int -> int -> int -> int -> t
val resize : (float -> unit) option -> t -> int -> int -> t
val rawimage : t -> rawimage
