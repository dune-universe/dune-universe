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

(* $Id: oColor.mli,v 1.1 2006/11/28 15:43:28 rousse Exp $*)

(** Class interface for Color *)

class rgbmap :
  Color.rgb Color.map ->
  object
    method add_color : Color.rgb -> int
    method add_colors : Color.rgb list -> int list
    method colormap : Color.rgb Color.map
    method find_exact : Color.rgb -> int
    method find_nearest : Color.rgb -> int
    method map : Color.rgb array
    method max : int
    method query_color : int -> Color.rgb
    method set_map : Color.rgb array -> unit
    method set_max : int -> unit
    method size : int
  end

class rgbamap :
  Color.rgba Color.map ->
  object
    method add_color : Color.rgba -> int
    method add_colors : Color.rgba list -> int list
    method colormap : Color.rgba Color.map
    method find_exact : Color.rgba -> int
    method find_nearest : Color.rgba -> int
    method map : Color.rgba array
    method max : int
    method query_color : int -> Color.rgba
    method set_map : Color.rgba array -> unit
    method set_max : int -> unit
    method size : int
  end

class cmykmap :
  Color.cmyk Color.map ->
  object
    method add_color : Color.cmyk -> int
    method add_colors : Color.cmyk list -> int list
    method colormap : Color.cmyk Color.map
    method find_exact : Color.cmyk -> int
    method find_nearest : Color.cmyk -> int
    method map : Color.cmyk array
    method max : int
    method query_color : int -> Color.cmyk
    method set_map : Color.cmyk array -> unit
    method set_max : int -> unit
    method size : int
  end
