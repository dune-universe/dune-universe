type t = private char
val a : t
val c : t
val g : t
val t : t

val all : t list
val card : int
val to_char : t -> char
val of_char : char -> t option
val of_char_exn : char -> t
val of_int : int -> t option
val of_int_exn : int -> t
val to_int : t -> int
val complement : t -> t
