(** Log-space computation *)

type t = private float

val zero : t

val one : t

val mul : t -> t -> t

val div : t -> t -> t

val min : t -> t -> t

val of_float : float -> t

val to_float : t -> float

include Basic_intf.Std with type t := t
