(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Support
open Term
open Signature

(** Structure that implements a table of all global features in the system *)

type t

val verbosity:  t -> int

val count:      t -> int
    (** The number of features in the table *)

val count_fgs:  int -> t -> int
    (** [count_fgs i ft] returns the number of formal generics of the feature [i] *)

val arity: int -> t -> int

val is_deferred: int -> t -> bool

val is_ghost_function: int -> t -> bool

val seed_function: t -> int -> int


val get_variant_seed: int -> int -> agens -> t -> int*agens

val add_variant: info -> int -> int -> agens -> t -> unit

val set_seed: int -> int -> agens -> t -> unit

val add_recognizer: term -> term -> int -> t -> unit
val recognizers: int -> t -> term list
val recognizer:  int -> t -> term
val filter_recognizers: term -> int -> t -> unit
val constructor_preconditions: int -> t -> term list
val add_constructor_preconditions: term list -> int -> t -> unit

val set_projector:  int -> int -> int -> t -> unit
val has_all_projectors: int -> t -> bool
val projectors: int -> t -> int array

val complexity: term -> int -> Tvars.t -> t -> int


val find_minimal_variants: int -> int -> t -> (int*agens) list
val find_new_variants: int -> t -> (int*agens) list

val string_of_signature: int -> t -> string

val is_ghost_term: term -> int -> t -> bool
val is_ghost_specification: Feature.Spec.t -> t -> bool


val false_constant: int -> term
val true_constant:  int -> term

val base_table: Module.Compile.t -> t

val class_table:  t -> Class_table.t
val compilation_context: t -> Module.Compile.t

val add_used_module:    Module.M.t -> t -> unit
val add_current_module: Module.M.t -> t -> unit
val set_interface_check: t -> unit

val is_private: t -> bool
   (** Are we within the implementation of a module? *)

val is_public:  t -> bool
   (** Are we either using or  checking an interface? *)

val is_interface_check:  t -> bool
   (** Are we checking an interface? *)

val is_interface_use:  t -> bool
   (** Are we using an interface? *)

val is_equality_index: int -> t -> bool

val split_equality: term -> int -> t -> int * int * term * term
    (** [split_equality t nbenv ft] returns the number of arguments, the
        equality in and the left and right hand side of an equality or raises
        [Not_found] if [t] is not an equality. *)

val is_equality: term -> int -> t -> bool
    (** [is_equality t nbenv ft] tells if the term [t] is an equality term *)


val equality_index: int -> t -> int
    (** [equality_index cls ft] returns the equality index of the class [cls]. *)

val equality_index_of_type: term -> Tvars.t -> t -> int
    (** [equality_index tp ft] returns the equality index of the type [tp]. *)


val feature_call: int -> int -> arguments -> agens -> t -> term

val definition_term: int -> int -> agens -> Tvars.t -> t -> int * int array * term
    (** [definition idx nb ft] returns the definition of the feature
        [idx]. Raises [Not_found] if feature [idx] has no definition *)

val has_definition_term: int -> t -> bool
val has_no_definition: int -> t -> bool

val is_inductive_set: int -> int -> t -> bool
val inductive_set: int -> term array -> agens -> int -> Tvars.t -> t -> term

val transformed_specifications: int -> int -> agens -> t -> term list

val feature_name: int -> t -> string

val is_deferred: int -> t -> bool
val tvars: int -> t -> Tvars.t
val signature0: int -> t -> Tvars.t * Sign.t
val argument_types: int -> agens -> int -> t -> types
val result_type: int -> agens -> int -> t -> type_term
val argument_names: int -> t -> int array
val body:         int -> t -> Feature.body

val is_pseudo_constructor:    int -> t -> bool
val is_constructor:    int -> t -> bool
val inductive_type:    int -> agens -> int -> t -> type_term
val evaluated_as_expression: term -> int -> Tvars.t -> t -> term
val induction_law:    int -> int -> t -> term
val is_pattern: int -> term -> int -> t -> bool
val pattern_subterms:  int -> term -> int -> t -> (int*term*int) list
val peer_constructors: int -> t -> IntSet.t
val unmatched_inspect_cases: (formals*term*term) array -> int -> int -> t
  -> (int * term list * term) list

val is_feature_visible: int -> t -> bool
val is_term_visible:    term -> int -> t -> bool

val owner: int -> t -> int
val dominant_formal_generic: int -> t -> int

val make_lambda:
  formals -> formals -> term list -> term -> type_term option
  -> int -> int -> t -> term
val make_application: term -> arguments -> type_term -> int -> t -> term
val beta_reduce:
  int -> term -> type_term -> term array -> int -> int -> t -> term

val substituted:
    term -> int -> int -> int
      -> arguments -> int -> agens -> Tvars.t -> t -> term
val specialized:      term -> int -> Tvars.t -> t -> term

val equality_term: term -> term -> int -> type_term -> Tvars.t -> t -> term
val implication: term -> term -> int -> term

val tuple_of_args:    arguments -> type_term -> int -> t -> term
val args_of_tuple:    term -> int -> t -> term array
val args_of_tuple_ext:term -> type_term -> int -> int -> t -> term array

val preconditions:  int -> int -> t -> int * int array * term list
val postconditions: int -> int -> t -> int * int array * term list
val count_postconditions: int -> t -> int
val function_property_assertions: int -> t -> term list
val function_property: int -> int -> int -> term array -> t -> term

val find_features: feature_name -> int -> t -> int list

val find_funcs: feature_name -> int -> t -> (int * Tvars.t * Sign.t) list
  (** [find_funcs fn nargs ft] finds all functions with name [fn] and [nargs]
      arguments in the feature table [ft] and returns the indices with the
      corresponding type variables and signatures. The signatures will be up-
      or downgraded if necessary and possible to match the requirement to have
      [nargs] arguments. *)


val find_proper_seed: info -> int -> t -> int*agens
val find_with_signature: feature_name withinfo -> Tvars.t -> Sign.t -> t -> int

val add_feature: feature_name withinfo -> Tvars.t -> int array -> Sign.t
  -> Feature.implementation -> t -> unit
val add_equality: int -> t -> unit
val update_specification: int -> Feature.Spec.t -> t -> unit
val hide_definition: int -> t -> unit
val export_feature: int -> t -> unit
val export_equality: int -> t -> unit

val involved_assertions: int -> t -> IntSet.t
val add_involved_assertion: int -> term -> t -> unit


val term_to_string: term -> bool -> bool -> int -> int array -> Tvars.t -> t -> string
val string_of_term_anon: term -> int -> t -> string

val check_interface: t -> unit

val downgrade_term: term -> int -> int -> t -> term

val domain_of_feature: int -> int -> agens -> Tvars.t -> t -> term

val equal_symmetry_term: unit -> term
val leibniz_term: unit -> term
