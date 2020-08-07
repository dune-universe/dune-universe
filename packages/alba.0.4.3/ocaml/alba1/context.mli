(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
 *)

(** Context with stacked declarations of formal arguments *)


open Signature
open Support
open Term
open Container

type t
val make:  Module.Compile.t -> t

val class_table: t  -> Class_table.t
val feature_table:t -> Feature_table.t

val add_used_module:    Module.M.t -> t -> unit
val add_current_module: Module.M.t -> t -> unit
val set_interface_check: t -> unit

val is_private:        t -> bool
val is_public:         t -> bool
val is_interface_check:t -> bool
val is_interface_use:  t -> bool
    (** Are we using an interface? *)

val verbosity: t -> int

val push:  entities list withinfo -> return_type -> bool -> bool -> bool -> t -> t
val push_untyped:     int array -> t -> t
val push_typed:   Formals.t -> Formals.t -> bool -> t -> t
val push_typed0:  Formals.t -> Formals.t -> t -> t
val push_empty: t -> t
val context_of_feature: int -> t -> t
val previous: t -> t
val pop:   t -> t

val is_global:   t -> bool
val is_local:    t -> bool
val is_toplevel: t -> bool
val depth:       t -> int
val arity:       t -> int
val info:        t -> info
val is_outer:    t -> t -> bool

val has_result:  t -> bool
val has_result_variable:  t -> bool
val result_type: t -> type_term

val count_type_variables: t -> int
    (** The number of cumulated type variables in this context and all
        preceeding contexts *)
val has_type_variables: t -> bool
val has_no_type_variables: t -> bool
val count_local_type_variables: t -> int
    (** The number of type variables in this context without all preceeding
        contexts *)

val count_formal_generics: t -> int
    (** The number of formal generics in this context and all preceeding
        contexts *)

val count_last_arguments:  t -> int
    (** The number of formal arguments in this context without the preceeding
        contexts *)

val count_last_variables:  t -> int
    (** The number of variables (arguments + result) in this context without
        the preceeding contexts *)

val count_last_formal_generics:  t -> int
    (** The number of formal generics in this context without the preceeding
        contexts *)

val count_last_type_variables: t -> int
    (** The number of type variables in this context without the preceeding
        contexts *)

val count_variables:  t -> int
    (** The number of variables in this context and all preceeding
        contexts *)

val varnames: t -> names

val count_all_type_variables: t -> int

val implication_index: t -> int
val is_equality_index: int -> t -> bool

val make_lambda:
  Formals.t -> Formals.t -> term list -> term -> type_term option -> t -> term
val make_application: term -> arguments -> type_term -> int -> t -> term
val beta_reduce:      int -> term -> type_term -> term array -> int -> t -> term

val quantified:      bool -> Formals.t -> Formals.t -> term -> t -> term
val all_quantified:  Formals.t -> Formals.t -> term -> t -> term
val some_quantified: Formals.t -> Formals.t -> term -> t -> term
val prenex_term:     term -> t -> term
val prenex_sort_term:term -> t -> term
val prenex_term_bubble_one:term -> t -> term

val variable_name: int -> t -> int
    (** The name of the [i]th variable argument *)

val variable_type: int -> t -> type_term
    (** The type of the [i]th variable argument *)

val variable_class: int -> t -> int
val variable_index: int -> t -> int

val is_constructor: int -> t -> bool
val is_pseudo_constructor: int -> t -> bool
val constructor_preconditions: int -> arguments -> agens -> t -> term list

val fgnames: t   -> int array

val fgnames:    t -> int array
val fgconcepts: t -> types
val local_argnames: t -> int array
val local_varnames: t -> int array
val local_argtypes:    t -> types
val local_vartypes:    t -> types
val local_formals:  t -> formals0
val local_fgs: t -> formals0
val argnames: t -> names
val argtypes: t -> types

val local_types_reduced: t -> types

val tvars: t -> Tvars.t

val ith_arguments_string: int -> t -> string
val local_arguments_string: t -> string
val arguments_string: t -> string
val boolean: t -> term

val function_class:  t -> int
val predicate_class: t -> int

val domain_type: type_term -> t -> type_term
val type_of_term: term -> t -> type_term
val class_of_term: term -> t -> int
val class_of_type: type_term -> t -> int
val tuple_type_of_types: types -> t -> type_term
val tuple_type_of_terms: arguments -> t -> type_term
val tuple_of_args:  arguments -> t -> term
val predicate_of_type: type_term -> t -> type_term
val predicate_of_term: term -> t -> type_term
val function_of_types: types -> type_term -> t -> type_term
val function_of_terms: arguments -> term -> t -> type_term

val args_of_tuple: term -> t -> term array

val update_types: type_term array -> t -> unit

val string_of_term0:      term -> bool -> bool -> int -> t -> string
val string_of_term:       term -> t -> string
val string_of_term_anon:  term -> int -> t -> string
val string_long_of_term:  term -> t -> string
val string_of_term_array: string -> term array -> t -> string
val string_of_arguments:  term array -> t -> string
val string_of_signature:  Sign.t -> t -> string
val string_of_type: type_term -> t -> string
val string_of_type_array: string -> agens -> t -> string
val string_of_ags: agens -> t -> string
val signature_string: t -> string
val named_signature_string: t -> string
val signature:  t -> Sign.t

val feature_signature: int -> t -> Tvars.t * Sign.t
val string_of_feature_signature: int -> t -> string

val transformed_term0: term -> int -> t -> t -> term
val transformed_term:  term -> t -> t -> term

val check_deferred: t -> unit

val is_case_match_expression: term -> t -> bool
val find_identifier: int ->          int -> t -> (int * Tvars.t * Sign.t) list
val find_features:   feature_name -> t -> int list
val find_feature:    feature_name -> int -> t -> (int * Tvars.t * Sign.t) list
val variable_data:   int -> t -> Tvars.t * Sign.t
val variable:        int -> t -> int * Tvars.t * Sign.t

val complexity: term -> t -> int

val split_general_implication_chain:
    term -> t -> Formals.t * Formals.t * term list * term
val split_equality: term -> int -> t -> int * int * term * term
val equality_term: term -> term -> t -> term
val and_term: term -> term -> t -> term
val implication_term: term -> term -> t -> term
val implication_chain: term list -> term -> t -> term
val definition_term: int -> int -> agens -> t -> int * int array * term
val arity:      int -> int -> t -> int
val is_inductive_set: int -> t -> bool
val inductive_set: term -> t -> term


val preconditions: int -> int -> t -> int * int array * term list
val postconditions: int -> int -> t -> int * int array * term list
val function_property: int -> int -> term array -> t -> term
val has_preconditions: int -> int -> t -> bool
val term_preconditions: term -> t -> term list

val domain_of_lambda:  Formals.t -> Formals.t -> term list  -> int -> t -> term
val domain_of_feature: int -> int -> agens -> t -> term

val existence_condition: term list -> t -> term
val uniqueness_condition: term list -> t -> term
val function_postconditions: int -> term list -> t -> term list

val get_type: type_t withinfo -> t -> type_term

val downgrade_term: term -> int -> t -> term

val arity_of_downgraded_type: type_term -> t -> int
val specialized: term -> t -> term
val type_of_term_full: term -> type_term option -> bool -> t -> type_term
val is_well_typed:    term -> t -> bool
