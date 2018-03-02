(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Jun Furuse, projet Cristal, INRIA Rocquencourt           *)
(*                                                                     *)
(*  Copyright 1999-2004,                                               *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  Distributed only by permission.                                    *)
(*                                                                     *)
(***********************************************************************)

(* $Id: Exp *)

type elt = int (* must be int32, but lablgtk uses int *)

(** ximage data structure with size information *)

type t = {
  width: int;
  height: int;
  data : Gdk.image;
}

val destroy : t -> unit
(** You need manual destroy *)

val create :
  kind:Gdk.Image.image_type -> visual:Gdk.visual ->
    width: int -> height: int -> t
(** Same as Gdk.Image.create, but with size info *)

val unsafe_get : t -> int -> int -> elt
val unsafe_set : t -> int -> int -> elt -> unit
val get : t -> int -> int -> elt
val set : t -> int -> int -> elt -> unit

val get_image : [>`drawable] Gobject.obj -> x:int -> y:int -> 
                   width:int -> height:int -> t
(* Same as Gdk.Image.get, but with size info *)

val of_image : Gdk.visual -> (float -> unit) option -> Images.t -> t

val get_mono_gc : Gdk.window -> Gdk.gc
val plain_mask : Gdk.window -> int -> int -> Gdk.bitmap
val pixmap_of : Gdk.window -> t -> Gdk.pixmap

val mask_of_image : Gdk.window -> Images.t -> Gdk.bitmap option
val pixmap_of_image :
  Gdk.window -> (float -> unit) option -> Images.t -> GDraw.pixmap

module Truecolor : sig
  val color_creator : Gdk.visual -> Images.rgb -> int
  val color_parser : Gdk.visual -> int -> Images.rgb
end
