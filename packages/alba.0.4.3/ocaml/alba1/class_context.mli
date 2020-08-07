open Term

type t

val make: Class_table.t -> t
val make_from_tvs: Tvars.t -> Class_table.t -> t
val make_from_fgs: names -> types -> Class_table.t -> t
val is_global: t -> bool
val is_local:  t -> bool
val push: names -> types -> t -> t
val pop: t -> t
val tvars: t -> Tvars.t
val class_table: t -> Class_table.t
val string_of_type: type_term -> t -> string
