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

type t

type joint

type knot

type direction

val print : Format.formatter -> t -> unit

val print_joint : Format.formatter -> joint -> unit

val print_dir : Format.formatter -> direction -> unit

val print_knot : Format.formatter -> knot -> unit

val knot : point -> knot

val vec_direction : point -> direction

val curl_direction : float -> direction

val no_direction : direction

val equalize_dir : direction * direction -> direction * direction

val line_joint : joint

val curve_joint : direction -> direction -> joint

val curve_no_inflex_joint : direction -> direction -> joint

val tension_joint : direction -> float -> float -> direction -> joint

val controls_joint : point -> point -> joint

val start : knot -> t

val concat : t -> joint -> knot -> t

val append : t -> joint -> t -> t

val cycle : joint -> t -> Spline_lib.path

val to_path : t -> Spline_lib.path

val from_path : Spline_lib.path -> t

module Approx : sig
  val lineto : point list -> Spline_lib.path

  val fullcircle : float -> Spline_lib.path
  (** fullcircle l is the circle of diameter l centered on (0, 0) *)

  (** fullcircle l is the circle of diameter l centered on (0, 0) *)
  val halfcirle : float -> Spline_lib.path
  (** halfcircle l is the upper half of a fullcircle of diameter l *)

  (** halfcircle l is the upper half of a fullcircle of diameter l *)
  val quartercircle : float -> Spline_lib.path
  (** quartercircle l is the first quadrant of a circle of diameter l *)

  (** quartercircle l is the first quadrant of a circle of diameter l *)
  val unitsquare : float -> Spline_lib.path
  (** unitsquare l : the path (0,0)--(l,0)--(l,l)--(0,l)--cycle *)
end
