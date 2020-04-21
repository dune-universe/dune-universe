type t

val empty: t

val (<+>): t -> t -> t

val substring: string -> int -> int -> t
val string: string -> t
val fill: int -> char -> t
val char: char -> t

val run: t -> string
