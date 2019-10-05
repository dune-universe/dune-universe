open Base

type t

type error = [`Not_enough_bytes]

val create : string -> t

val has_more_bytes : t -> bool

val read_byte : t -> (char, [> error]) Result.t

val read_bytes : t -> int -> (string, [> error]) Result.t

val read_while : t -> (char -> bool) -> string
