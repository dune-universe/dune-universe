open! Core

type t

val create : unit -> t
val samples : t -> int
val total : t -> float
val min : t -> float
val max : t -> float
val mean : t -> float
val var : t -> Float.t
val stdev : t -> float
val update_in_place : t -> float -> unit
val copy : t -> t

