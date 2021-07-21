(*-----------------------------------------------------------------------
 ** Copyright (C) - Verimag.
 ** This file may only be copied under the terms of the CeCill
 ** Public License
 **-----------------------------------------------------------------------
 **
 ** File: bddd.mli
 ** Main author: erwan.jahier@univ-grenoble-alpes.fr
 *)

(** Bdd Drawer. *)

(** Machinery to perform a draw in a bdd. 

    ZZZ : To be used by Solver only !!
*)

type t  
val tbl_to_string : t -> string

exception No_numeric_solution	of t

val draw_in_bdd : t -> Var.env_in -> Var.env -> int -> string -> Exp.var list ->
  Bdd.t -> Bdd.t -> t * Var.env * Store.t' * Store.p
  (** [draw_in_bdd memory verbose_level ctx_msg state vars bdd comb]
      returns a draw of the Boolean variables as well as a range based
      and a polyhedron based representation of numeric constraints
      (cf. the [Store] module).
      
      Side effect: this is where the solution number tables is filled in.

      Raises [No_numeric_solution].
  *)

open Sol_nb

val formula_to_bdd : t -> Var.env_in -> Var.env -> string -> int ->
  Exp.formula -> t * Bdd.t

val index_to_linear_constraint : t -> int -> Constraint.t
val get_index_from_linear_constraint : t -> Constraint.t -> int
val num_to_gne: t -> Var.env_in -> Var.env -> string -> int -> Exp.num -> t * Gne.t
val eval_int_expr: t -> Exp.num -> string -> Var.env_in -> Var.env  -> int -> int option


val clear : t -> t
  
val sol_number : t -> Bdd.t -> sol_nb * sol_nb
  (** [sol_number bdd] returns the solution number in the [then] and [else]
    branches of [bdd]. *)

(**/**)


val add_snt_entry : t -> Bdd.t -> sol_nb * sol_nb -> t
(** mofifies an entry. usefull whenever we realize latter that a bdd
 represents a formula that is unsatifiable for numerical reasons. *)


val init_snt : unit -> t
