(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: formula_to_bdd.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(**)
(**  Encoding formula and expressions into bdds. *)

type t
val init : unit -> t
val tbl_to_string : t -> string
  
val f : t -> Var.env_in -> Var.env -> string -> int -> Exp.formula -> t * Bdd.t
  (** [Formula_to_bdd.f input memory ctx_msg verbosity_level f]
      returns the bdd of [f] where [input] and pre variables have
      been repaced by their values. [ctx_msg] is used for printing
      source information in case of errors.
      
      ZZZ this function fills in local tables that are used later on during
      the random toss (e.g., via index_to_linear_constraint). 
      Hence bdd should be build by this module. 
  *)


val num_to_gne: t -> Var.env_in -> Var.env -> string -> int -> Exp.num -> t * Gne.t

val eval_int_expr: t -> Exp.num -> string -> Var.env_in -> Var.env  -> int -> int option


(** Clean-up all the internal tables that migth have been filled
  by (non-regression) assertions.
*)
val clear_all : t -> t

(** Clean-up the cache tables that contain information that
    depends on input or pre. Indeed, this information is very likely
    to change (especially because floats) at each step, therefore we do
    not keep that information to avoid memory problems.
*)
val clear_step : t -> t


(* Returns the atomic formula of an index. *)
val index_to_linear_constraint : t -> int -> Constraint.t
val get_index_from_linear_constraint : t -> Constraint.t -> int

