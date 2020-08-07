open Term

type t
val make: Feature_table.t -> t
val make_from_arguments: names -> types -> names -> types -> Feature_table.t -> t
val is_global: t -> bool
val is_local:  t -> bool
val push: names -> types -> names -> types -> t -> t
val pop:  t -> t
val formals: t -> Formals.t
val count_variables: t -> int
val variable_type: int -> t -> type_term
val tvars: t -> Tvars.t
val feature_table: t -> Feature_table.t
val class_table: t -> Class_table.t
val split_general_implication_chain:
  term -> t -> int * formals * formals * term list * term
val string_of_term: term -> t -> string
val string_of_signature: int -> t -> string
