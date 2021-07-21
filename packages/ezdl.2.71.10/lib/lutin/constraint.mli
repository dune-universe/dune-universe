(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: constraint.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Internal representation of constraints used in formula, namely,
  linear constraints over Booleans, integers, and floats. *)


(** Normalised linear constraints. *)
(* exported *)
type ineq =
  | GZ      of Ne.t (** expr >  0 *)
  | GeqZ    of Ne.t (** expr >= 0 *)

(* exported *)
type t =
  | Bv      of Exp.var (** booleans  *)
  | EqZ     of Ne.t   (** expr  = 0 *)
  | Ineq    of ineq   (** > or >= *)

(** Returns the number of variables involved in the constraint *)
val dimension : t -> int
val dimension_ineq : ineq -> int

(** returns the vars appearing in a constraint *)
val get_vars : t -> string list
val get_vars_ineq : ineq -> string list

(** [apply_subst cstr s] applies [s] to [cstr]. Note that the result could
  have been multiplied by a constant. *)
val apply_subst : Ne.subst -> t -> t
val apply_subst_ineq : Ne.subst -> ineq -> ineq
val apply_substl : Ne.subst list -> t -> t
val apply_substl_ineq : Ne.subst list -> ineq -> ineq

(** [neg_ineq cstr] returns the negation of an inequality constraint. *)
val neg_ineq : ineq -> ineq

(** [neg_ineq cstr] returns [cstr] where its Ne.t is replaces by its opposite. *)
val opp_ineq : ineq -> ineq

(** Pretty printing. *)
val to_string : t -> string
val to_string_verbose : t -> string
val print : t -> unit

val ineq_to_string : ineq -> string
val print_ineq : ineq -> unit

val eval_ineq : Var.num_subst list -> ineq ->  bool
