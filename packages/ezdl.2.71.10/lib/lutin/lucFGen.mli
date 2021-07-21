(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: fGen.mli

--> modified to fit the FGgen interface
    the only exported thing is the "get" whihch creates a FGen.t
*)


val get : Var.env_in -> Prog.state ref -> FGen.t list

(* NO LONGER EXPORTED ------- 
(** Runtime Automaton. *)

type t
(** This abstract datatype is used to hold sub-automaton of the
  (static) parsed Lucky automaton (cf the [LucProg] module). It
  contains the sub-automaton made of transitions accessible from the
  current node, where formulas and weights have been evaluated w.r.t
  current inputs and memories. Such automata are recomputed at each
  step and are used to draw the formula that will be used to generate the
  Lucky outputs.

  nb: If the resulting automaton contains inner cycles (namely,
  cycles made of transient nodes), an error is raised. Such
  transitional cycle are forbidden to avoid loops during execution.

  To sum-up, to build such Runtime (sub)automaton, we:

  - compute the transitions accessible from the current node, i.e.,
  by transitively traverse the static automaton from the current
  node, until stable nodes are reached.

  - evaluate formulas w.r.t. inputs and memories;

  - evaluate weights expressions w.r.t. inputs and memories;

  - remove transitions labelled by unsatisfiable formulas;

  - check that there is no transient cycle;

  - performs the automata composition, for automata that share output
    variables.

    The composition of automata is {b not} done at the beginning and
    once for all at the level of static automaton, but here, once
    runtime automata have been computed and formulas have been
    evaluated. The reasons are that, since automata product is
    notoriously exponential, the smallest the automata are, the
    better. Hence, we limit the product explosion in two ways:

  + sub-[automata] are of course much smaller;
  + since we have waited for the time when inputs and memories are
    known, some more transitions are removed because some formulas
    are then known to be unsatisfiable.

*)


val get : Var.env_in -> Prog.state -> t list
  (** [get input state] computes the formula generator from then
      current control points.
      
      nb: get returns the list of formula generator dealing with
      distinct output variables. *)

  
(** Raised by choose_one_formula no more transitions can be taken *)
exception NoMoreFormula

(** Raised by choose_one_formula when a Prog.Stop is chosen (For Lutin) *)
exception NormalStop of string

val choose_one_formula : t ->  t * Exp.formula * Prog.ctrl_state

(** [choose_one_formula run_aut] draws a formula accessible from 
    the current node, and returns:
    - a failure continuation to be used if one wants to draw another formula.
    - A formula (nb1).
    - The program point that corresponds to the choice that has been made.
*)


val get_all_formula : t -> (Exp.formula) list
  (** gets all the formulas accessible from the current node. *)

*)
