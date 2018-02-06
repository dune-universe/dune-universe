type value =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of value list
  | `O of (string * value) list ]

val sexp_of_value : value -> Base.Sexp.t
val value_of_sexp : Base.Sexp.t -> value
val compare : value -> value -> int
val hash : value -> int

type t =
  [ `A of value list
  | `O of (string * value) list ]

val sexp_of_t : t -> Base.Sexp.t
val t_of_sexp : Base.Sexp.t -> t
val compare : t -> t -> int
val hash : t -> int
