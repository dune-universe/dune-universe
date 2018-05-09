type elt = float
type t

val len: t -> int
val get: t -> int -> elt
val set: t -> int -> elt -> unit
val create: int -> t
