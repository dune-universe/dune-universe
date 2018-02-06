type t =
  [ `Assoc of (string * t) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `Intlit of string
  | `List of t list
  | `Null
  | `String of string
  | `Tuple of t list
  | `Variant of string * t option ]

val sexp_of_t : t -> Base.Sexp.t
val t_of_sexp : Base.Sexp.t -> t
val compare : t -> t -> int
val hash : t -> int
