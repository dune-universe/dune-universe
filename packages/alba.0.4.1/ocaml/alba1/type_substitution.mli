open Term


exception Reject

type t

val count: t -> int
val greatest_plus1: t -> int
val has:   int -> t -> bool
val get:   int -> t -> type_term
val put:   int -> type_term -> t -> unit
val array: int -> t -> agens
val unify: type_term -> type_term -> t -> unit
val unify_types: types -> types -> t -> unit
val make:  int -> Tvars.t -> Tvars.t -> Class_table.t -> t
val make_equal:
  type_term -> Tvars.t -> type_term -> Tvars.t -> Class_table.t -> agens
