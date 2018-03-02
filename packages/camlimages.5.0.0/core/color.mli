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

(* $Id: color.mli,v 1.1 2006/11/28 15:43:28 rousse Exp $ *)

(** Definition of colormaps, i.e. mappings from real RGB colors to
 integers. The integer corresponding to a color [c] is an index [i]
 into a vector of colors whose [i]th element is [c]. *)

exception Too_many_colors
(** An exception raised when too many number of colors are used
    for a given color model. *)

type 'a map = { mutable max : int; mutable map : 'a array }
(** Colormap *)

(** Colormap manipulation functions *)

val size : 'a map -> int
    (** Returns the size of a colormap. *)

val find_exact : 'a map -> 'a -> int
    (** Finds a color in the colormap and returns its color index.
       Raises exception [Not_found] if the color is not in the colormap. *)

val add_color : 'a map -> 'a -> int
    (** Add a new color into the given colormap and return its index.
       If the color is already in the colormap, it is not
       added again, and the corresponding color index is returned. *)

val add_colors : 'a map -> 'a list -> int list
    (** Add the list of new colors into the given colormap and return 
       their indices. If a color is already in the colormap, it is not
       added again, and the corresponding color index is returned. *)

val copy : 'a map -> 'a map
    (** Copy a colormap *)

(*
val find_nearest : 'a map -> 'a -> int
    (* [find_nearest m c] finds the color [co] that is the nearest to
    [c] color into the colormap [m].
    Returns the color index of [co] and the distance between [c]
    and [co]. *)
    (* this is defined inside each color model implementation *)
*)

module type S = sig
  type t

  val square_distance : t -> t -> int
  (** Square distance of colors *)
    
  val plus : t -> t -> t
  (** Adding colors.  No overflow is considered. *)
    
  val minus : t -> t -> t
  (** Subtracting colors.  No underflow is considered. *)
    
  val size : t map -> int
  (** size of the color map *)
    
  val find_exact : t map -> t -> int
  (** Find the given color in the colormap and returns the color index.
      [Not_found] may be raised.
  *)

  val add_color : t map -> t -> int
  (** [add_color map c] adds [c] to [map] if [c] is not in [map].
      The function returns the color index for [c].
      It may raise [Too_many_colors].
  *)
        
  val add_colors : t map -> t list -> int list
  (** [add_colors] is as same as [add_color] but it adds multiple colors *)

  val find_nearest : t map -> t -> int
  (** [find_nearest map c] returns a color index of [map] which is
      the nearest to [c].  The color distance is given by [square_distance].
  *)
end
    
type rgb = { mutable r : int; mutable g : int; mutable b : int }
(** R(ed), G(reen), B(lue) representation of colors. *)

module Rgb : S with type t = rgb
(** Colormap for RGB *)
  
type rgba = { color: rgb; mutable alpha : int; }
(** RGB with alpha (transparent) information *)

module Rgba : sig
  include S with type t = rgba
  val merge : t -> t -> t
  (** [merge src dst] merges colors.  
      If [src] is completely opaque it overrides [dst] completely.
      No overflow or underflow may happen. *)
end                
(** Colormap for RGBA *)


type cmyk = { mutable c : int; mutable m : int; mutable y : int; mutable k : int; }
 (** Cyan Magenta Yellow blacK color model *)

module Cmyk : S with type t = cmyk
(** Colormap for CMYK *)

(** Rgb specialized functions (for backward compatibility) *)

val rgb_square_distance : rgb -> rgb -> int
(** Compute the distance between two colours. *)

val plus : rgb -> rgb -> rgb
val minus : rgb -> rgb -> rgb

val brightness : rgb -> int

(** Color name parser *)

val color_parse : string -> rgb
(** Color name parser function.

  It queries the name in the color name database given by the file
  [Camlimages.path_rgb_txt].  It also understands the following color format:
  
    "#rrggbb"       where r,g and b are [0-9a-fA-F]
    "#rrrrggggbbbb" where r,g and b are [0-9a-fA-F]

  It may raise [Failure] if not found.
*)

val colormap_parse : string array -> rgb array * int
(** Same as [color_parse] but work for multiple names.

    If a color of the result has a minus value for its R component,
    it is considered transparent and replaced by r=0 g=255 b=0.
    The function returns the last transparent color index.
*)
