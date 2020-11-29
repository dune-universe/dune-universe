(* Create a lexbuf that reads code from Markdown fenced code blocks *)

val read_lexbuf : string -> in_channel -> Lexing.lexbuf
