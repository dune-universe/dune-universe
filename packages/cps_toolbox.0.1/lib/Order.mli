type t =
  | EQ
  | LT
  | GT

val int : int -> int -> t
val string : string -> string -> t
