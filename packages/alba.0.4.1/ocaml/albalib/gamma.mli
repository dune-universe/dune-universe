type t

val count: t -> int

val is_valid_index: int -> t -> bool

val index_of_level: int -> t -> int

val level_of_index: int -> t -> int


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

val standard: unit -> t

val compute: Term.t -> t -> Term.t

val push_local: string -> Term.typ -> t -> t
