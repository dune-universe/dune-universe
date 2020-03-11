open Support
open Container


type term =
    Variable    of int
  | VAppl       of int * arguments * arguments * bool (* fidx, args, ags, oo *)
  | Application of term * arguments * bool (* fterm, args, inop *)
  | Lam         of formals * formals * term list * term * type_term option
  | QExp        of formals * formals * term * bool (* args, fgs, t, is_all *)
  | Ifexp       of term * term * term
  | Asexp       of term * types * term
  | Inspect     of term * (formals * term * term) array
  | Indset      of int * type_term * arguments (* name, type, rules *)
and names      = int array
and arguments  = term array
and agens      = type_term array
and types      = type_term array
and formals0   = names * arguments
and formals   = (int,type_term) Array2.t
and type_term  = term
and info_term  = term withinfo

exception Term_capture
exception Empty_term

module TermMap: Map.S with type key = term

val empty_term: term
val empty_formals: formals0
val standard_substitution: int -> term array
val is_standard_substitution: term array -> bool
val make_type: int -> arguments -> type_term
val split_type: type_term -> int * agens
val count_formals: formals0 -> int
val string_of_names: names -> string


module Term: sig

  val is_variable_i: term -> int -> bool

  val to_string: term -> string

  val variable:    term -> int
  val is_variable: term -> bool
  val is_variable_below: int -> term -> bool
  val is_argument: term -> int -> bool

  val is_permutation: term array -> bool
  val invert_permutation: term array -> term array
  val nodes0: term -> int -> int array -> int
  val nodes: term -> int

  val fold_with_level: ('a -> int -> int -> 'a) -> 'a -> term -> 'a
  val fold: ('a -> int -> 'a) -> 'a -> term -> 'a
  val fold_arguments: ('a -> int -> 'a) -> 'a -> term -> int -> 'a

  val least_free: term -> int

  val greatestp1_arg: term -> int -> int

  val split_variables: term -> int -> IntSet.t * IntSet.t

  val variables_filtered: term -> (int->bool) -> IntSet.t

  val free_variables: term -> int -> IntSet.t

  val bound_variables: term -> int -> IntSet.t

  val range_variables: term -> int -> int -> IntSet.t

  val used_variables_0:     term -> int -> int list -> int list
  val used_variables:       term -> int -> int list
  val used_variables_filtered_0:
    term -> (int -> bool) -> bool -> int list -> int list
  val used_variables_filtered:   term -> (int -> bool) -> bool -> int list
  val used_variables_array_filtered:   term array -> (int -> bool) -> int list
  val used_variables_from:  term -> int -> bool -> int list
  val used_variables_transform: term -> int -> int array * int array
  val unused_transform:     formals0 -> int -> formals0 -> term ->
    formals0 * arguments * formals0 * arguments
  val remove_unused: formals0 -> int -> formals0 -> term ->
                     formals0 * formals0 * term
  val equivalent: term -> term -> bool

  val equivalent_list: term list -> term list -> bool

  val equivalent_array: term array -> term array -> bool

  val map_free: (int->int) -> (int->int) -> term -> term

  val lambda_inner: term -> int -> term

  val lambda_inner_map: term -> int IntMap.t -> term

  val shift_from : int -> int -> int -> int -> term -> term
  val shift: int -> int -> term -> term

  val up_type:   int -> type_term -> type_term
  val down_type: int -> type_term -> type_term

  val down_from: int -> int -> term -> term

  val down: int -> term -> term

  val up_from: int->int->term->term

  val up: int->term->term

  val array_up: int -> term array -> term array

  val subst0_from: term -> int -> int -> arguments -> int -> int -> arguments -> term

  val subst0: term -> int -> term array -> int -> term array -> term

  val subst_from: term -> int -> int -> arguments -> term
  val subst:      term -> int -> arguments -> term
  val subst_array:arguments -> int -> arguments -> arguments

  val apply0: term -> term array -> term array -> term
  val apply:  term -> term array -> term

  val lambda_split: term -> formals * formals * term list * term * type_term option

  val qlambda_split_0: term -> formals * formals * term * bool
  val qlambda_split: term -> formals * formals * term * bool

  val unary: int -> term -> term

  val unary_split: term -> int -> term

  val quantified: bool -> formals -> formals -> term -> term

  val all_quantified:  formals -> formals -> term -> term

  val some_quantified:  formals -> term -> term

  val quantifier_split: term -> bool -> formals * formals * term

  val all_quantifier_split:  term-> formals * formals * term
  val all_quantifier_split_1:term-> formals * formals * term

  val some_quantifier_split: term -> formals * term

  val is_all_quantified: term -> bool
  val is_generic: term -> bool

  val pattern: formals -> term -> term
  val pattern_split: term -> formals * term
  val case_split: term -> term -> formals * term * term

  val binary: int -> term -> term -> term

  val binary_split_0: term -> int * term * term

  val binary_split: term -> int -> term * term

  val split_implication_chain: term -> int -> term list * term
  val make_implication_chain:  term list -> term -> int -> term

  val split_left_binop_chain: term -> int -> term list

  val split_general_implication_chain:
    term -> int -> formals * formals * term list * term

  val closure_rule:   int -> term -> term -> term
  val induction_rule: int -> int -> term -> term -> term
    -> formals * term list * term
  val induction_law:  int -> term -> term -> type_term -> type_term -> term
  val prepend_names:  names -> names -> names
  val prenex:            term -> int -> int -> int -> term
  val prenex_sort:       term -> int -> int -> int -> term
  val prenex_bubble_one: term -> int -> int -> int -> term

end

module Formals:
sig
  type t = (int,type_term) Array2.t
  val empty: t
  val make:  names -> types -> t
  val from_pair: (names*types) -> t
  val copy:  t -> t
  val count: t -> int
  val names: t -> names
  val types: t -> types
  val name:  int -> t -> int
  val is_equivalent: t -> t -> bool
  val typ:   int -> t -> type_term
  val map:   (type_term -> type_term) -> t -> t
  val sub:   int -> int -> t -> t
  val prepend: t -> t -> t
  val formals: t -> formals0
end


module Term_sub: sig
  type t
  val to_string:      t -> string
  val count:          t -> int
  val for_all:        (int -> term -> bool) -> t -> bool
  val iter:           (int -> term -> unit) -> t -> unit
  val fold:           (int -> term -> 'a -> 'a) -> t -> 'a -> 'a
  val map:            (term -> term) -> t -> t
  val is_identity:    t -> bool
  val is_injective:   t -> bool
  val empty:          t
  val identity:       int -> t
  val is_empty:       t -> bool
  val singleton:      int -> term -> t
  val find:           int -> t -> term
  val mem:            int -> t -> bool
  val add:            int -> term ->  t -> t
  val merge:          t -> t -> t
  val to_list:        t -> (int*term) list
  val arguments:      int -> t -> term array
  val filled_arguments: int -> int -> t -> term array
  val has_only_variables: t -> bool
end
