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

(* $Id: fttext.mli,v 1.1 2007/01/18 10:29:57 rousse Exp $ *)

open Color
open Freetype

(* the type for actual drawing functions and some samples *)
type 'a drawer = 'a -> int -> 'a
val func_darken_only : rgb drawer
val func_red_only : rgb drawer

val unicode_of_latin : string -> int array
val unicode_of_euc_japan : string -> int array

(* general drawing function *)
val draw_rotated_text :
    float ->
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    int array ->
    unit

val draw_rotated_glyphs :
    float ->
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    char_index array ->
    unit

val draw_text :
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    int array ->
    unit

val draw_glyphs :
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    char_index array ->
    unit

val draw_mono_rotated_text :
    float ->
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    int array ->
    unit

val draw_mono_rotated_glyphs :
    float ->
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    char_index array ->
    unit

val draw_mono_text :
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    int array ->
    unit

val draw_mono_glyphs :
    (int -> int -> int -> unit) ->
    face ->
    int -> int ->
    char_index array ->
    unit

module type T = sig
  type t
  type elt

  val create : int -> int -> t
  val destroy : t -> unit
  val get : t -> int -> int -> elt
  val set : t -> int -> int -> elt -> unit
  val unsafe_get : t -> int -> int -> elt
  val unsafe_set : t -> int -> int -> elt -> unit
end

module Make(T : T) : sig
  (* Draw texts *)
  (* [draw face drawer image x y text] *)
  (* Draw a text on image at (x,y), using drawer function *)
  (* text must be encoded by some encoder and translated into int array *)

  val draw_text : Freetype.face -> T.elt drawer -> T.t ->
    int -> int -> int array -> unit

  (* Draw rotated texts *)
  (* [draw_rotated face drawer image x y r text] *)
  (* Draw a text on image at (x,y) rotated r *)
  (* Drawn text is automatically smoothed *)

  val draw_rotated_text : Freetype.face -> T.elt drawer -> T.t->
    int -> int -> float -> int array -> unit

  val draw_glyphs : Freetype.face -> T.elt drawer -> T.t ->
    int -> int -> char_index array -> unit

  val draw_rotated_glyphs : Freetype.face -> T.elt drawer -> T.t->
    int -> int -> float -> char_index array -> unit

  (* Monochrome (black/white) drawing *)
  val draw_mono_text : Freetype.face -> T.elt drawer -> T.t ->
    int -> int -> int array -> unit

  val draw_mono_rotated_text : Freetype.face -> T.elt drawer -> T.t->
    int -> int -> float -> int array -> unit

  val draw_mono_glyphs : Freetype.face -> T.elt drawer -> T.t ->
    int -> int -> char_index array -> unit

  val draw_mono_rotated_glyphs : Freetype.face -> T.elt drawer -> T.t->
    int -> int -> float -> char_index array -> unit

end

(* Get the size information of text *)
val size :
  Freetype.face -> int array -> float * float * float * float

val size_of_glyphs :
  Freetype.face -> char_index array -> float * float * float * float

(* Vector based *)
val vector_gen :
  (Freetype.face -> 'a -> 'b list -> float * float) ->
  bool ->
  float ->
  (Freetype.outline_contents -> unit) ->
  Freetype.face -> float -> float -> 'a array -> unit

val vector_text :
  bool -> (Freetype.outline_contents -> unit) ->
  Freetype.face -> float -> float -> float -> int array -> unit

val vector_glyphs :
  bool -> (Freetype.outline_contents -> unit) ->
  Freetype.face ->
  float -> float -> float -> Freetype.char_index array -> unit
