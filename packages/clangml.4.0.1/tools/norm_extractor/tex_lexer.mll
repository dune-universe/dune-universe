{
  type section_title = {
      level : int;
      ident : string;
      title : string;
    }

  type lexeme =
    | EOF
    | Begin_codeblock
    | Rsec of section_title
}

rule main = parse
| '%' {
  skip_line_comment None lexbuf;
  main lexbuf
}
| "\\begin{codeblock}" {

  Begin_codeblock
}
| "\\rSec" (['0' - '9']+ as level)
  '[' ([^ ']']* as ident) ']'
  '{' {
  let title_buffer = Buffer.create 17 in
  title title_buffer lexbuf;
  let title = Buffer.contents title_buffer in
  Rsec { level = int_of_string level; ident; title }
}
| '\n' {
  Lexing.new_line lexbuf;
  main lexbuf
}
| eof {
  EOF
}
| _ {
  main lexbuf
}

and title buffer = parse
| '{' {
  title buffer lexbuf;
  title buffer lexbuf
}
| '}' { () }
| '\\' (['a' - 'z' 'A' - 'Z']*) {
  title buffer lexbuf
}
| eof {
  failwith "Unterminated title"
}
| _ as c {
  Buffer.add_char buffer c;
  title buffer lexbuf
}

and code_block buffer = parse 
| "\\end{codeblock}" {
  ()
}
| '\n' {
  Lexing.new_line lexbuf;
  Buffer.add_char buffer '\n';
  code_block buffer lexbuf
}
| '@' {
  code_block_latex_escape buffer lexbuf
}
| _ as c {
  Buffer.add_char buffer c;
  code_block buffer lexbuf
}
| eof {
  failwith "Missing \\end{codeblock}"
}

and code_block_latex_escape buffer = parse
| '@' {
  code_block buffer lexbuf
}
| '\n' {
  Lexing.new_line lexbuf;
  Buffer.add_char buffer '\n';
  code_block_latex_escape buffer lexbuf
}
| '\\' (['a' - 'z' 'A' - 'Z']* as command) {
  begin
    match command with
    | "textbackslash" ->
        Buffer.add_char buffer '\\';
    | _ -> ()
  end;
  code_block_latex_escape buffer lexbuf
}
| '{' | '}' | '$' {
  code_block_latex_escape buffer lexbuf
}
| _ as c {
  Buffer.add_char buffer c;
  code_block_latex_escape buffer lexbuf
}
| eof {
  failwith "Missing @"
}

and skip_line_comment buffer_opt = parse
| '\n' {
  buffer_opt |> Option.iter begin fun buffer ->
    Buffer.add_char buffer '\n'
  end;
  Lexing.new_line lexbuf;
  ()
}
| eof {
  ()
}
| _ as c {
  buffer_opt |> Option.iter begin fun buffer ->
    Buffer.add_char buffer c
  end;
  skip_line_comment buffer_opt lexbuf
}


