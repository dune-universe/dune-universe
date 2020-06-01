open! Base
open! Import

type t

val of_string : string -> t
val to_constructor_string : t -> string
val to_lowercase_string : t -> string
