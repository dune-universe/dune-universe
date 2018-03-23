open! Core

type t [@@deriving sexp_of]

val multipart_boundary : t -> Boundary.t option

val is_multipart : t -> bool
val is_message_rfc822 : t -> bool

val last : Headers.t -> t option
val default : parent:t option -> t
