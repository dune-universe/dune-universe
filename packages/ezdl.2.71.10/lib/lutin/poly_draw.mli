(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: polyDraw.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Drawing into convex Polyhedron. *)

(** This module implements algorithms to draw uniformally in a convex
  polyhedron given a set of generators. *)


type point = float list


(** This one is cheaper, but a little bit unfair (i.e., not uniform). *)
val draw_point_cheap : point list -> point

(** Draws n dictinct points among a list of points l,
  or |l| if |l|<n (typically, vertices).
*)
val draw_n_distinct_points : int -> point list -> point list

(** Draws n points among a list of points (any point may be drawn
  several times).
*)
val draw_n_points : int -> point list -> point list



(** {4 A fair but possibly inefficient drawing function.} *)

type poly_wrap
(**  Parallelogram envelopping a convex Polyhedron defined by
  a set of generators.
*)

val compute_poly_wrap : point list -> poly_wrap

(** Draw one point in a poly_wrap. 

    BEWARE: the point returned by one_point_poly_wrap is not 
    necessary a solution!
*)
val one_point_poly_wrap : poly_wrap -> point

(** Draw n points in a poly_wrap. *)
val n_points_poly_wrap : poly_wrap -> int -> point list




(**/**)

val poly_wrap_volume : poly_wrap -> float

val print_points : point list -> unit

type vector = float list
val scal_prod : vector -> vector -> float
val vect_norm : vector -> float
