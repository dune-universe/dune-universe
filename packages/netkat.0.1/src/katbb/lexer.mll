{
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

let whitespace = ' ' | '\t'
let newline = "\r\n" | '\r' | '\n'
let ident = ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_']*

rule next_token = parse
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | "(*"
    { comment 0 lexbuf; next_token lexbuf }

  (* actual tokens here *)
  | "0" { ZERO }
  | "1" { ONE }
  | "`" { BTICK }
  | '?' { QMARK }
  | '!' { BANG }
  | '(' { LPAR }
  | ')' { RPAR }
  | '+' { PLUS }
  | ';' { SCOLON }
  | '~' { TILDE }
  | '*' { STAR }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | ident as var { VAR var }

  (* EOF/illegal token *)
  | eof { EOF }
  | _ as c { illegal c }

and comment nesting = parse
  | "(*"
    { comment (nesting+1) lexbuf }
  | "*)"
    { if nesting > 0 then comment (nesting - 1) lexbuf }
  | eof
    { failwith "[lexer] unterminated comment at EOF" }
  | _
    { comment nesting lexbuf }

