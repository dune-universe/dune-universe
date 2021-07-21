(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: polyhedron.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Miscellaneous functions over polyhedra. *)


(** Convex range *)
type range =
    RangeI of Num.num * Num.num    (* min and max *)
  | RangeF of float * float (* Ditto for floats *)


(**
  [build_poly_list_from_delayed_cstr verb tbl cl] puts the constraint
  [cl] in several polyhedra (one polyhedra per group of constraints
  holding on disjunct set of variables), taking into account
  constraints already made on vars (which can be found in [tbl]).

  Also returns the list of variables involved in [cl] as well as a new
  [tbl] where the delayed constraints have been removed from the range based
  store.
*)
val build_poly_list_from_delayed_cstr :
  int -> range Util.StringMap.t -> Constraint.ineq list ->
      ( range Util.StringMap.t
      * Var.name list
      * (Var.vnt list * Poly.t * (int -> string) * Constraint.ineq list) list)


type point = float list

val get_vertices : Poly.t ->  (int -> string) -> point list

(** Transform a point into a list of substitutions, performing the necessary
  conversion (which are wrong from float to integers !).
*)
val point_to_subst_list : Var.vnt list -> (int -> string) -> point ->
  Var.num_subst list


val point_is_in_poly : Var.vnt list -> point -> (int -> string) ->
      Constraint.ineq list -> bool

(**/**)

val clean_internal_tbl : unit -> unit

(** Returns the volume (approximation) of a Polyhedron using
  a monte-carlo like method. *)
val volume : Poly.t -> (int -> string) -> float
val range_to_string : range -> string
