(** A dynamically-sized array, similar to std::vector in C++. *)

open! Import

type t =
  { mutable data : Bits.t array
  ; mutable length : int
  }
[@@deriving sexp_of, compare]

val create : unit -> t
val init : int -> (int -> Bits.t) -> t
val length : t -> int
val get : t -> int -> Bits.t
val set : t -> int -> Bits.t -> unit
