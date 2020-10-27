type t = {
  lstart: Lexing.position;
  lend: Lexing.position;
}

val curr : Lexing.lexbuf -> t
val dummy_loc : t

val loc : Lexing.position -> Lexing.position -> t

val print_loc: Format.formatter -> t -> unit
