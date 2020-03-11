open Term

module Spec: sig
  type t
  val make_empty:      int array -> t
  val make_func_def:   int array -> term option -> term list -> t
  val make_func_spec:  int array -> term list -> term list -> t
  val is_empty:        t -> bool
  val equivalent:      t -> t -> bool
  val is_consistent: t -> t -> bool
  val has_definition_term:  t -> bool
  val definition_term_opt:  t -> term option
  val definition_term:      t -> term
  val count_arguments: t -> int
  val names:           t -> int * int array
  val has_preconditions: t -> bool
  val preconditions:   t -> term list
  val count_postconditions: t -> int
  val has_postconditions: t -> bool
  val postcondition:    int -> t -> term
  val postconditions:   t -> term list
  val has_no_definition: t -> bool
  val without_definition: t -> t
end

type implementation =
    Builtin
  | Deferred
  | Empty
  (*| Code of ???*)


type body = Spec.t * implementation
