(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: prog.mli
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)


(* XXX should be abstract! *)
type atomic_ctrl_state = string

type ctrl_state = atomic_ctrl_state list


(** Weigths, once evaluated (no more memories nor inputs) *)
type dyn_weight = V of int | Infin

(** A wt (weigthed tree) is a n-ary tree encoding the set of possible
    formula for the next step. *)
type wt = children Util.StringMap.t * string  (* the top-level node of the wt *) 
and children =
  | Children of (dyn_weight * string) list
  | Leave of Exp.formula * atomic_ctrl_state
  | Stop of string

(** The Digested Lucky program  (form the static part of the state). 
    Holds a list of independant programs (i.e., they don't share any I/O).
*)
type t = {
  initial_ctrl_state : ctrl_state list;
  in_vars  : Exp.var list;
  out_vars : Exp.var list;
  loc_vars : Exp.var list;
  ext_func_tbl : Exp.ext_func_tbl;
  memories_names   : (string * Exp.var) list ; (* redundant, but useful *)
  bool_vars_to_gen : Exp.var list list ;
  num_vars_to_gen  : Exp.var list list ;
  output_var_names : Var.name list list ; (* redundant, but useful *)
  reactive : bool ; (* A scorie to remove. At least, it should be a list of bool *)
  
(* Computes the weigthed tree from the static program *)
  get_wtl :  Var.env_in -> state -> ctrl_state -> wt list;

  is_final : ctrl_state list -> bool;

(* to generate a pdf dot file for the source. Returns the error code 
    of the system call (dot -Tpdf). *)
  gen_dot : ctrl_state list -> ctrl_state list -> string -> int
}

(** The dynamic part of the state (that changes at each cycle). *)
and dynamic_state_fields = {
  (* memory  : Var.subst list; *)
  memory  : Var.env;
  ctrl_state : ctrl_state list;
  (* input : Var.subst list; *)
  last_input : Var.env;
  last_output : Var.env;
  last_local : Var.env;
  snt: Solver.t;
  verbose : int
}
and state = {
  d : dynamic_state_fields ;
  s : t (* The static part *)
}

val memory_of_state : state -> Var.env
val last_values_of_state : state -> (Var.env * Var.env * Var.env)

val compute_weight : Exp.weight -> Var.env_in -> state -> dyn_weight

(** Pretty-printing *)
val print_wt : wt -> unit
val ctrl_state_to_string : ctrl_state -> string
val ctrl_state_list_to_string_list : ctrl_state -> string list

(** Used for printing information about the current control point in error messages. *)
val ctrl_state_to_string_long : ctrl_state list -> string


(**/**)

val print_mem : state -> unit
val string_to_atomic_ctrl_state : string -> atomic_ctrl_state

