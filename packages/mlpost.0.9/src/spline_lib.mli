(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

type point = Point_lib.t

type abscissa = Spline.abscissa

type path_ = { pl : Spline.t list; cycle : bool }

type path = Point of point | Path of path_

val is_closed : path -> bool

val is_a_point : path -> point option

val create : point -> point -> point -> point -> path
(** create a b c d return a path with :
      - point a as the starting point,
      - point b as its control point,
      - point d as the ending point,
      - point c as its control point *)

val create_point : point -> path
(** create a path consisting of a single point *)

val create_line : point -> point -> path
(** create a straight line between two points *)

val create_lines : point list -> path
(** create a path consisting of straight lines connecting the points
    in argument *)

val close : path -> path
(** close a path *)

val min_abscissa : path -> abscissa

val max_abscissa : path -> abscissa

val length : path -> int
(** number of vertex *)

val add_end : path -> point -> point -> path
(** add_end p a b return the path p with one more spline at the end.*)

val add_end_line : path -> point -> path

val add_end_spline : path -> point -> point -> point -> path

val append : path -> point -> point -> path -> path

val reverse : path -> path
(** reverse p return the path p reversed *)

(*val union : path -> path -> path (** union p1 p2 return the union of
  path p1 and p2. [min_abscissa p1;max_abscissa p1] are points of p1,
  ]max_abscissa p1;max_abscissa p1+max_abscissa p2-min_abscissa p2]
  are points of p2 *)

  val union_conv : path -> path -> (abscissa -> abscissa)
*)
val one_intersection : path -> path -> abscissa * abscissa

val intersection : path -> path -> (abscissa * abscissa) list
(** intersection p1 p2 return a list of pair of abscissa. In each
    pairs (a1,a2), a1 (resp. a2) is the abscissa in p1 (resp. p2) of
    one intersection point between p1 and p2. Additionnal point of
    intersection (two point for only one real intersection) can
    appear in degenerate case. *)

val fold_left :
  ('a -> point -> point -> point -> point -> 'a) -> 'a -> path -> 'a
(** fold on all the splines of a path *)

val iter : (point -> point -> point -> point -> unit) -> path -> unit
(** iter on all the splines of a path *)

val cut_before : path -> path -> path

val cut_after : path -> path -> path
(** remove the part of a path before the first intersection
      or after the last*)

val split : path -> abscissa -> path * path

val subpath : path -> abscissa -> abscissa -> path

val direction_of_abscissa : path -> abscissa -> point

val abscissa_to_point : path -> abscissa -> point

val bounding_box : path -> point * point

val unprecise_bounding_box : path -> point * point

val dist_min_point : path -> point -> float * abscissa
(** [dist_min_point p s] computes the minimal distance of [p] to [s],
    as well as the abscissa which corresponds to this minimal
    distance; the return value is [distance, abscissa].
*)

val dist_min_path : path -> path -> float * (abscissa * abscissa)
(** [dist_min_path p1 p2] computes the minimal distance of [p1] to
    [p2], as well as the two abscissa which correspond to this minimal
    distance; the return value is [distance, (abscissa_on_p1,
    abscissa_on_p2)].
*)

val translate : point -> path -> path

val transform : Matrix.t -> path -> path

val buildcycle : path -> path -> path

val of_bounding_box : point * point -> path

val print : Format.formatter -> path -> unit

val print_splines : Format.formatter -> Spline.t list -> unit
