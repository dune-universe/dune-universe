(** An abstract type for port-name strings. *)


open! Import

type t [@@deriving compare, sexp_of]

include Comparable.S with type t := t
include Equal.S with type t := t
include Stringable.S with type t := t
