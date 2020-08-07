type t =
  | Proposition
  | Any
  | Box


val type_of: t -> t option
val product: t -> t -> t
val sub: t -> t -> bool
val equal: t -> t -> bool
