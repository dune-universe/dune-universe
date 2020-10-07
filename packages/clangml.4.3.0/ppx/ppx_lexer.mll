{
 type antiquotation_type =
   | Typename
   | Decl
   | Expression of string

 let type_buffer = Buffer.create 17
}

let ident = ['a' - 'z' '_'] ['A' - 'Z' 'a' - 'z' '0' - '9' '_']*

rule main = parse
| "[%" (ident as antiquotation_type) {
  let antiquotation_type =
    match antiquotation_type with
    | "class" | "typename" -> Typename
    | "decl" -> Decl
    | _ -> Expression antiquotation_type in
  Some antiquotation_type
}
| "[%(" {
  let start = lexbuf.lex_start_p in
  parse_parenthesis lexbuf;
  let antiquotation_type = Buffer.contents type_buffer in
  Buffer.clear type_buffer;
  lexbuf.lex_start_p <- start;
  Some (Expression antiquotation_type)
}
| "//" {
  ignore_line_comment lexbuf;
  main lexbuf
}
| "/*" {
  ignore_block_comment lexbuf;
  main lexbuf
}
| "\"" {
  ignore_string lexbuf;
  main lexbuf
}
| eof {
  None
}
| _ {
  main lexbuf
}
and ignore_line_comment = parse
| '\n' | eof {
  ()
}
| _ {
  ignore_line_comment lexbuf
}
and ignore_block_comment = parse
| "*/" | eof {
  ()
}
| _ {
  ignore_block_comment lexbuf
}
and ignore_string = parse
| "\"" | eof {
  ()
}
| "\\" _ {
  ignore_string lexbuf
}
| _ {
  ignore_string lexbuf
}
and parse_parenthesis = parse
| ")" | eof {
  ()
}
| "(" {
  Buffer.add_char type_buffer '(';
  parse_parenthesis lexbuf;
  Buffer.add_char type_buffer ')';
  parse_parenthesis lexbuf
}
| _ as c {
  Buffer.add_char type_buffer c;
  parse_parenthesis lexbuf
}
