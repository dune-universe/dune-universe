(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: solver.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Formula solver.

    It is a wrapper around Bddd (and Fair_Bddd), which are not supposed
    to be used elsewhere.

    ZZZ: 
    - init_snt should be called once before using all the other functions !
    - clear_snt can be called or not (it simply clears cache tables)
    - is_satisfiable/is_satisfiable_bdd is a lightweight version of 
       [solve_formula], that do not build solution number table; it is used
       to avoid calling [solve_formula] when we do not really need to draw 
       some solutions
    - solve_formula is the main function, that  
      - solve the formula 
      - fills in sol nb table
      - performs a (Boolean) draw (numerics are drawn latter)
  
*)

(* The solver monad *)
type t = Bddd.t (* should be abstract! *)
val init : unit -> t
val tbl_to_string : t -> string

val is_satisfiable : t -> 
  Var.env_in -> Var.env -> int -> string -> Exp.formula -> string -> t * bool
  (** [is_satisfiable input memory verbose_level msg f] suceeds iff the
      formula [f], once evaluated w.r.t. [input] and memories, is
      satisfiable from the Boolean point of vue. At this level, each
      numeric constraint is view as a Boolean variable. The numeric
      satisfiability will be checked later, during the draw. The reason
      for that is that we do not want to build unnecessary polyhedra
      because it is very expensive.
  *)

val is_satisfiable_bdd : t -> 
  Var.env_in -> Var.env -> int -> string -> Exp.formula -> string -> t * bool * Bdd.t
(* idem, but returns the corresponding bdd *)

val solve_formula : t -> Var.env_in -> Var.env -> int -> string -> 
  Var.name list list -> Thickness.formula_draw_nb -> Thickness.numeric -> 
  Exp.formula -> Exp.var list -> Exp.formula -> t * (Var.env * Var.env) list
  (** [solve_formula input memory verbose_level msg output_var_names
      fdn tn bools nums f] randomly assigns [tn] values to variables
      in [bools] and [nums] (which ought to contains all output and
      local variables occurring in the formula [f]). It returns a
      list (of length [tn]) of pairs, where the left hand side of the
      pair contains the output vars, and the right hand side contains
      the local vars.

      The Boolean list of variables is encoded in a formula for
      performance reasons: [solve_formula] is likely to be called
      significantly often with the same arguments so that it is worth
      precomputing this encoding before.

      Side effect: fills in the solution number table
  *)

val eval_int_expr:  t -> Exp.num -> string -> Var.env_in -> Var.env -> int
     -> int option

(**/**)

(** The bool flag can be set to true to remove path to the false node.

    ZZZ : solve_formula should have been called once to be able to use
   this function with this flag set to [true], as it uses the solution
   number table.  *)
val print_bdd_with_dot: t -> Bdd.t -> string -> bool -> unit
  

