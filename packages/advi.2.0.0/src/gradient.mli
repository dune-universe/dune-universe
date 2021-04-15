(***********************************************************************)
(*                                                                     *)
(*                             Active-DVI                              *)
(*                                                                     *)
(*                   Projet Cristal, INRIA Rocquencourt                *)
(*                                                                     *)
(*  Copyright 2003 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Lesser General Public License.          *)
(*                                                                     *)
(*  Pierre Weis                                                        *)
(*                                                                     *)
(*  Filling rectangles and arcs with gradients.                        *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type x = int
and y = int;;
(** The types of coordinates in the library. *)

type r = int
and rx = int
and ry = int;;
(** The types of radius of graphical entities. *)

type from_angle = int
and to_angle = int;;
(** The types of angles of graphical entities. *)

type from_color = Graphics.color
and to_color = Graphics.color;;
(** The types of colors of graphical entities. *)

type w = int
and h = int;;
(** The types of width and height of graphical entities. *)

type rectangle_gradient_mode =
   | Rect_Horizontal of from_color * to_color
      (** Horizontal gradient. *)
   | Rect_Vertical of from_color * to_color
      (** Vertical gradient. *)
   | Rect_Diagonal1 of from_color * to_color
      (** Gradient along the first bissector of the rectangle. *)
   | Rect_Diagonal2 of from_color * to_color
      (** Gradient along the second bissector of the rectangle. *)
   | Rect_Centered of x * y * from_color * to_color
      (** Centered gradient. *)
   | Rect_Circular of x * y * from_color * to_color
      (** Circular gradient. *)
;;

(** Gradient modes to fill rectangles. Each mode specifies a gradient
    from [from_color] to [to_color], with the corresponding method.
    The [x] and [y] coordinates of constructors [Rect_Circular] and
    [Rect_Centered] are those of the gradient starting point. This
    point is supposed to be inside the rectangle. *)

type arc_gradient_mode =
   | Arc_Horizontal of from_color * to_color
      (** Horizontal gradient from c1 to c2 *)
   | Arc_Vertical of from_color * to_color
      (** Vertical gradient *)
   | Arc_Circular of from_color * to_color
      (** Circular gradient *)
;;
(** Gradient modes to fill arcs. Each mode specifies a gradient
    from [from_color] to [to_color], with the corresponding method. *)

val grad_arc : arc_gradient_mode ->
  x -> y -> rx -> ry -> from_angle -> to_angle -> unit;;
(** Fill an arc with the specified [arc_gradient_mode]. *)

val grad_circle : arc_gradient_mode -> x -> y -> r -> unit;;
(** Fill a circle with the specified [arc_gradient_mode]. *)

val grad_rect : rectangle_gradient_mode -> x -> y -> w -> h -> unit;;
(** Fill a rectangle with the specified  [rectangle_gradient_mode]. *)
