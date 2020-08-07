type definition =
  | Axiom
  | Assumption
  | Builtin_type of string
  | Builtin of string * Term.Value.t
  | Definition of Term.t
  | Inductive_type of int * int
  | Constructor of int * int * int


type entry = {
    name: string;
    typ: Term.typ;
    definition: definition
  }



type t

val empty: t

val count: t -> int

val is_valid_index: int -> t -> bool

val index_of_level: int -> t -> int

val level_of_index: int -> t -> int

val level_forall: (int -> bool) -> Term.t -> t -> bool
val level_has:    (int -> bool) -> Term.t -> t -> bool

val entry: int -> t -> entry
(** [entry level c] *)


val add_axiom: string -> Term.typ -> t -> t


val add_builtin_type:     string -> string -> Term.typ -> t -> t
val add_builtin_function: string -> string -> Term.typ -> t -> t



val raw_type_at_level: int -> t -> Term.typ

(** [type_at_level level c] type of the entry at [level]. *)
val type_at_level: int -> t -> Term.typ


val int_type: t -> Term.typ


val type_of_literal: Term.Value.t -> t -> Term.typ
val type_of_variable: int -> t -> Term.typ



val name_at_level: int -> t -> string

val name_of_index: int -> t -> string

val variable_at_level: int -> t -> Term.t

val definition_term: int -> t -> Term.t option

val inductive_at_level: int -> t -> Inductive.t option

val compute: Term.t -> t -> Term.t

val push_local: string -> Term.typ -> t -> t

val add_definition: string -> Term.typ -> Term.t -> t -> t

val add_inductive: Inductive.t -> t -> t
