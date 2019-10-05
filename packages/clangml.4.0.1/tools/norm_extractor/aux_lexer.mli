type lexeme =
  | EOF
  | Reference of { ident : string; number : string }

val main : Lexing.lexbuf -> lexeme
