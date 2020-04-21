(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Container
open Term
open Proof


type t

val context: t -> Context.t
val class_table: t   -> Class_table.t
val feature_table: t -> Feature_table.t

val is_private:         t -> bool
val is_public:          t -> bool
val is_interface_use:   t -> bool
val is_interface_check: t -> bool
val is_tracing:         t -> bool
val is_trace_extended:  t -> bool
val verbosity:          t -> int
val trace_prefix:       t -> string
val trace_prefix_0:     t -> string
val add_used_module:    Module.M.t -> t -> unit
val add_current_module: Module.M.t -> t -> unit
val set_interface_check: t -> unit

val has_result: t -> bool
val has_result_variable: t -> bool

val is_global: t -> bool
val is_local:  t -> bool
val is_toplevel: t -> bool

val string_of_term: term -> t -> string
val string_long_of_term: term -> t -> string
val string_of_term_anon: term -> int -> t -> string
val string_of_term_i: int -> t -> string
val string_long_of_term_i: int -> t -> string
val string_of_type: type_term -> t -> string
val arguments_string: t -> string

val is_well_typed: term -> t -> bool
val prenex_term: term -> t -> term
val predicate_of_type: type_term -> t -> type_term

val make: Module.Compile.t -> t

val push: entities list withinfo -> return_type -> bool -> bool -> bool -> t -> t
val push_typed:   Formals.t -> Formals.t -> bool -> t -> t
val push_typed0:  Formals.t -> Formals.t -> t -> t
val push_empty:   t -> t
val pop:          t -> t

val depth:     t -> int

val find:               term -> t -> int
val has:                term -> t -> bool
val add_assumption:     term -> bool -> t -> int
val add_axiom:          term -> t -> int
val add_mp:             int -> int -> bool -> t -> int
val try_add_beta_reduced:int -> bool -> t -> int
val add_beta_reduced:   int -> bool -> t -> int
val add_beta_redex:     term -> int -> bool -> t -> int
val add_some_elim:      int -> bool -> t -> int
val add_some_elim_specialized: int -> term -> bool -> t -> int
val specialized:        int -> arguments -> agens -> int -> t -> int
val has_work:           t -> bool
val work:               t -> int list
val add_to_work:        int -> t -> unit
val clear_work:         t -> unit
val close_step:         t -> unit
val close:              t -> unit
val close_assumptions:  t -> unit
val discharged:         int  -> t -> term * proof_term
val discharged_bubbled: int  -> t -> term * proof_term
val add_proved_term:    term -> proof_term -> bool -> t -> int
val add_proved_with_delta: term -> proof_term -> int -> t -> int
val add_proved:         term -> proof_term -> t -> int
val add_proved_list:    bool -> int -> (term*proof_term) list -> t -> unit
val remove_or_remap:    IntSet.t -> t -> unit
val premises:           int -> t -> (term*bool) list
val previous_schematics:int  -> t -> int list
val trying_goal:        term -> t -> unit
val failed_goal:        term -> t -> unit
val proved_goal:        term -> t -> unit

val find_schematic:     term -> int -> t -> int * agens
val find_match:         term -> t -> int
val find_goal:          term -> t -> int
    (** Find a term which exactly matches the goal or which can be specialized to
        match the goal. Add the specialization if necessary and return the index
        of the term which exactly matches or raise [Not_found]. *)

val find_backward_goal: term -> IntSet.t -> t -> int list
    (** Add all possible fully specialized backward rules whose target matches the
        goal by specialization, expansion, beta reduction.
        Return the backward rules whose target matches the goal exactly. *)

val split_implication:  term -> t -> term * term
val implication_chain:  term list -> term -> t -> term
val split_general_implication_chain:
    term -> t -> Formals.t * Formals.t * term list * term
val beta_reduce: int -> term -> type_term -> term array -> int -> t -> term
val beta_reduce_term: term -> t -> term
val count:          t -> int
val count_previous: t -> int
val count_global:   t -> int

val count_variables:      t -> int
val count_last_arguments: t -> int
val count_all_type_variables: t -> int
val count_type_variables: t -> int
val local_argnames: t -> names
val local_formals: t -> formals0
val local_fgs: t -> formals0
val tvars: t -> Tvars.t

val term:           int -> t -> term
val is_assumption:  int -> t -> bool
val count_local_assumptions: t -> int
val rule_data: int -> t -> Rule_data.t

val negation:       term -> t -> term
val implication:    term -> term -> t -> term
val disjunction:    term -> term -> t -> term
val false_constant: t -> term

val assumptions:        t -> term list
val assumptions_chain:  term -> t -> term
val assumption_indices: t -> int list
val assumptions_for_variables: int array -> int list -> term -> t
                               -> int list * int list * int
val check_deferred: t -> unit
val owner:          t -> int

val specialize_induction_law: int -> term -> int -> t -> int
val add_induction_law0:  int -> t -> unit
val add_set_induction_law: term -> term -> term -> t -> int

val check_interface: t -> unit

val excluded_middle:     t -> int
val or_elimination:      t -> int
val indirect_proof_law:  t -> int
val has_excluded_middle: t -> bool
val has_or_elimination:  t -> bool
val expand_variable_definitions: int -> t-> unit
