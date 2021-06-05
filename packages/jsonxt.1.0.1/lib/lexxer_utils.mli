
exception Lex_error of string

val error_pos : Lexing.lexbuf -> (int * int * int)
val error_pos_msg : Lexing.lexbuf -> string
val lex_error : string -> 'a
val string2num : string -> Tokens.token
val update_pos : Lexing.lexbuf -> unit
val unescape_string : string -> string
