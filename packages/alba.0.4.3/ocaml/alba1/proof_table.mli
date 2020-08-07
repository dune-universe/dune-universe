(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Term
open Proof
open Support
open Container

type t

val context: t -> Context.t
val class_table: t -> Class_table.t

val is_private:         t -> bool
val is_public:          t -> bool
val is_interface_use:   t -> bool
val is_interface_check: t -> bool
val add_used_module:    Module.M.t -> t -> unit
val add_current_module: Module.M.t -> t -> unit
val set_interface_check: t -> unit


val depth:       t -> int
val is_global:   t -> bool
val is_local:    t -> bool
val is_toplevel: t -> bool
val count:       t -> int
val count_previous: t -> int
val count_global:   t -> int
val count_variables:t -> int
val has_result:     t -> bool
val has_result_variable:     t -> bool
val count_last_arguments:  t -> int
val count_last_variables:  t -> int
val count_last_type_variables:  t -> int
val local_argnames: t -> int array
val local_formals:  t -> formals0
val local_fgs:      t -> formals0
val last_arguments_string: t -> string
val names:       t -> int array
val imp_id:      t -> int

val prenex_term: term -> t -> term
val split_implication: term -> t -> term * term
val split_all_quantified: term -> t -> Formals.t * Formals.t * term
val split_some_quantified: term -> t -> Formals.t * term
val split_equality: term -> int -> t -> int * term * term
val implication: term -> term -> t -> term
val all_quantified:  Formals.t -> Formals.t -> term -> t -> term
val implication_chain: term list -> term -> t -> term
val someelim:  int -> t -> term

val set_induction_law: term -> t -> term
val is_inductive_set: int -> t -> bool
val inductive_set: term -> t -> term

val type_induction_law: int -> t -> term

val term:          int -> t -> term * Context.t
val proof_term:    int -> t -> proof_term
val string_of_term_i: int -> t -> string
val string_long_of_term_i: int -> t -> string
val nbenv_term:    int -> t -> int
val local_term:    int -> t -> term
val transformed_to_current: term -> int -> t -> term
val is_assumption: int -> t -> bool
val variant:       int -> int -> int -> t -> term

val specialized: int -> term array -> int -> t -> term
val beta_reduce: int -> term -> type_term -> term array -> int -> t -> term
val apply_term:  term -> term array -> int -> t -> term
val reconstruct_evaluation: Eval.t -> t -> term*term

val make: Module.Compile.t -> t
val push: entities list withinfo -> return_type -> bool -> bool -> bool -> t -> t
val push_typed:   Formals.t -> Formals.t -> bool -> t -> t
val push_empty:   t -> t
val pop:  t -> t

val definition_term: int -> int -> agens -> t -> int * int array * term
val arity:      int -> int -> t -> int

val is_proof_pair:  term -> proof_term -> t -> bool

val add_proved_0:      term -> proof_term -> t -> unit
val add_proved:        term -> proof_term -> int -> t -> unit

val add_axiom:      term -> t -> unit
val add_assumption: term -> t -> unit
val add_mp:         term -> int -> int -> t -> unit
val add_eval:       term -> int -> Eval.t -> t -> unit
val add_eval_backward:   term -> term -> Eval.t -> t -> unit
val add_witness:    term -> int -> names -> types -> term -> term array -> t -> unit
val add_someelim:   int -> term -> t -> unit
val add_specialize: term -> int -> arguments -> agens -> t -> unit
val add_inherited:  term -> int -> int -> int -> t -> unit

val is_local_assumption: int -> t -> bool
val count_local_assumptions: t -> int
val assumptions:  t   -> term list
val assumption_indices: t -> int list
val discharged:           int -> t -> term * proof_term
val discharged_bubbled:   int -> t -> term * proof_term
val print_pt:     proof_term -> t -> unit
