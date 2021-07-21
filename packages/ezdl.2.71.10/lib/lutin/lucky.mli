(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: lucky.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

(**)
(** This module defines Lucky top-level functions [step] and [try]. *)



(** A solution (of a lucky step) is made of an instanciation of each
  controlleable (i.e., local and output) variables of the current
  set of lucky automata.
*)
type solution = Var.env_out * Var.env_loc	

(** To indicate whether the point used to perform the step is
  drawn inside, at edges, or at vertices of the convex hull of
  solutions; the step mode is used iff at least one controllable
  variable is numeric.  *)
type step_mode =  StepInside | StepEdges | StepVertices


(** [env_step step_mode input state] performs a step and returns
    the new automaton state as well as an instanciation of each
    controllable variables.

    Raises FGen.NoMoreFormula when no more step can be done
*)
val env_step : step_mode -> Var.env_in -> Prog.state ref -> FGen.t list -> 
  solution


  (** [env_try thickness input state] does basically the same things
    as [env_step], except that it does not return any state, and that
    it returns several solutions.  The number of returned solutions
    depends on the test [thickness] (cf the [Thickness] module).

      Raises FGen.NoMoreFormula when no more try can be done
  *)
val env_try : Thickness.t -> Var.env_in -> Prog.state -> FGen.t list 
  -> FGen.t list * solution list
