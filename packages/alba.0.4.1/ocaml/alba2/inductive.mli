open Alba2_common

module Term = Term2

(** Data structures to represent inductive types.*)

type carg_class =
  | Normal          (* Normal argument, no recursion *)
  | Positive        (* Potentially recursive, contains a parameter positively *)
  | Recursive       (* Recursive argument *)


type param = string option * Term.typ * bool

type constructor

type t

val ndproduct: Term.typ list -> Term.typ

val nparams: t -> int
val ntypes:  t -> int
val nconstructors: int -> t -> int

val params0: t -> Term.arguments
val types0:  t -> Term.gamma
val positive_parameters: t -> bool array

val params:  t -> Term.arguments
val types:   t -> Term.gamma
val itype:   int -> t -> Term.fname_type
val sort:    int -> t -> Sorts.t

val name: int -> t -> Term.fname
val is_restricted: int -> t -> bool
val is_restriction_consistent: int -> t -> bool
val all_args_propositions: int -> t -> bool

val cname: int -> int -> t -> Term.fname
val constructor_base_index: int -> t -> int
val constructors: int -> t -> (Term.fname * Term.typ * carg_class list) array
val ctype:   int -> int -> t -> Term.fname_type
val constructor_arguments: int -> int -> t -> carg_class list


val cmake: Term.fname -> carg_class list -> Term.typ -> constructor

val make_simple: Term.fname -> param list -> Term.typ -> Sorts.t -> bool
                 -> constructor list -> t


val make_false: t
val make_true: t
val make_and: t
val make_or: t
val make_equal: t
val make_exist: t
val make_accessible: t
val make_natural: t
val make_list: t
val make_tree: int -> t
