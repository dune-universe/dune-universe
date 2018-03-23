open Core_kernel

type t [@@deriving hash, sexp_of]

include Comparable.S_binable with type t := t
include Hashable with type t := t

val create : unit -> t

val to_int_exn : t -> int
val to_string  : t -> string
