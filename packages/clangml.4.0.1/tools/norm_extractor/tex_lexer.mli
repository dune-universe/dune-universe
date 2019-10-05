type section_title = {
    level : int;
    ident : string;
    title : string;
  }

type lexeme =
  | EOF
  | Begin_codeblock
  | Rsec of section_title

val main : Lexing.lexbuf -> lexeme

val code_block : Buffer.t -> Lexing.lexbuf -> unit
