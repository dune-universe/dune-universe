open! Core

type t [@@deriving sexp]

val create : ?num_samples_to_keep:int -> unit -> t

val add : t -> int -> unit
val percentile : t -> float -> int Or_error.t
val percentile_exn : t -> float -> int
val distribution : t -> int list

