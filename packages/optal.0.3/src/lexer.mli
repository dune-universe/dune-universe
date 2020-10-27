type error =
  | Unexpected_character of char
  | Int_overflow of string

exception Error of error * Loc.t

val token : Lexing.lexbuf -> Parser.token

val report_error : Format.formatter -> error -> unit
