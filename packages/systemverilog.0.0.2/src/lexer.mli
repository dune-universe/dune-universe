exception SyntaxError of string
val next_line : Lexing.lexbuf -> unit
val __ocaml_lex_tables : Lexing.lex_tables
val read : Lexing.lexbuf -> Parser.token
val __ocaml_lex_read_rec : Lexing.lexbuf -> int -> Parser.token
val read_string : Buffer.t -> Lexing.lexbuf -> Parser.token
val __ocaml_lex_read_string_rec :
  Buffer.t -> Lexing.lexbuf -> int -> Parser.token
