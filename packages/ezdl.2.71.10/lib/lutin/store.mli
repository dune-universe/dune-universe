(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: store.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Numeric constraint store.

  The algorithm is (roughly) the following: constraints are added one
  by one to the store. Constraints that involve more than one
  variable are delayed; the other ones are handled by a range-based
  (interval) store. Each time an equality constraint is added, we
  substitute it in the list of delayed variables. Some of the delayed
  constraints can therefore turn out to be of dimension 1; they are
  thus awaken and added to the range-based store.

  At the end of that process, namely, when all constraints have been
  added, the constraints in the delayed list are transformed into
  polyhedra. Independant polyhedra should be detected here as some
  polyhedron operations are exponential into their dimension.
  Moreover, it is easy to do.
*)


type range_store =  Polyhedron.range Util.StringMap.t


(* Should probably made abstract, but it made things heavy when 
   I needed to split that files (cf draw.ml) *)
type t = {
  (*
    This field is used to store where each variable ranges.  It is
    set to [Unsat] when the system becomes unsatisfiable, namely,
    when the range for one of the variable becomes empty.

    Some variables are represented by Ranges (polyhedron of dimension
    one). Some others by plain Polyhedron. The idea is that, at bdd
    leaves, if it remains some delayed constraints, we switch to a
    polyhedron representation.  *)
  var : vars_domain ;

  (*
    This field is used to substitute a variable by an expression. This
    is to deal with equalities: when an equality is encountered,
    we can remove one dimension by putting the equality into a
    such a substitution.

    Then we apply it to all the other relations. the value of the
    substituted variable is then obtained once the other var have
    been drawn.

    We add an element to this list if
    - an equality is encountered during the drawing/bdd-traversal
    - whenever a variable become bounded (1) after a constraint is
      added to the store

    (1) i.e., when the interval is reduced to one single point
  *)
  substl : Ne.subst list;

  (*
    When the dimension of an atomic formula is greater than 1, we
    delay its addition to the store until an equality makes it a
    constraint of dimension 1 (i.e., it contains only 1 var). At bdd
    leaves, if this list is not empty, it means that the current
    formula cannot be solved with an interval based solver.
    In that case, we use a polyhedron solver.
  *)
  delay : Constraint.ineq list ;

  (* Variables that have been constrained. If a formula has not been
    constraint when the draw is done, we give it its default value if any.
  *)
  untouched : Exp.var list
}
and 
  vars_domain =
    Unsat of Constraint.t * t 
      (* contains the store and the constraint that makes it 
	 unsatisfiable (in order to give feedback to the user). *)
  | Range of range_store


val create : Exp.var list -> t
  (** [Store.create varl] returns an initial store (the universe) for
    the variable contained in [varl]. *)



val add_constraint : t -> Formula_to_bdd.t -> Constraint.t -> t
  (** [add_constraint s c] returns the store [s] with the numeric
    constraint [c] added.
  *)

val is_store_satisfiable : int -> t -> bool
(** Returns false iff one of the variable domain is empty. *)


(*-----------------------------------------------------------------------*)


type p = (Var.vnt list * Poly.t * (int -> string) * Constraint.ineq list) list
  (** Convex Polyhedron. *)


(** contains basically the same info as [t] with a few fields removed *)
type t' = {
  range : range_store ;
  substl' : Ne.subst list;
  untouched' : Exp.var list
}


exception No_polyedral_solution
(** Raised by the 2 functions below *)

val switch_to_polyhedron_representation : int -> t -> t' * p 
  (** [switch_to_polyhedron_representation store] is called when all
    the constraints have been handled, and returns a range-based and a
    list of polyhedra (cf the algorithm above).
 
    Raises No_polyedral_solution if that conversion fails
  *)

(**/**)

(*-----------------------------------------------------------------------*)
val compute_volume : int -> t -> float
(** [compute_volume verb store] computes an approximation of the volume 
  of [store]. 

    Raises [No_polyedral_solution]
*)

val get_untouched_var : t' -> Exp.var list

(** [remove_var_from_range_store store var] removes [var] from the store. *)
val remove_var_from_range_store : t' -> Exp.var -> t'

(* This function is used to clean-up the store from unconstrainted
    vars that have a default values. *)


(*-----------------------------------------------------------------------*)
(** Pretty printing *)

val to_string : t -> string
val t'_to_string : t' -> string
val print_store : t -> unit

