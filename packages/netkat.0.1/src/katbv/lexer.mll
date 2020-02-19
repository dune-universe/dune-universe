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

let mask_rgx = ['0' '1' '?']+

rule next_token = parse
  | whitespace+
    { next_token lexbuf }
  | newline
    { Lexing.new_line lexbuf; next_token lexbuf }
  | "(*"
    { comment 0 lexbuf; next_token lexbuf }

  (* actual tokens here *)
  | "F" { ZERO }
  | "T" { ONE }
  | '(' { LPAR }
  | ')' { RPAR }
  | '+' { PLUS }
  | ';' { SCOLON }
  | '~' { TILDE }
  | '*' { STAR }
  | '=' { EQUALS }
  | ":=" { ASSIGN }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | mask_rgx as mask { MASK mask }
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

