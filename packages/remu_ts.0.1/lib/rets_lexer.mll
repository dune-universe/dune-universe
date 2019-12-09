{
open Lexing
open Rets_parser
exception SyntaxError of string
}

let digit = ['0'-'9']
let decimal = digit+
let white = [' ' '\t' '\n' '\r']+
let id = ['a'-'z' 'A'-'Z' '_'] ['-' '%' 'a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = parse
  | decimal { DECIMAL (int_of_string (Lexing.lexeme lexbuf)) }
  | "store" { STORE }
  | "nom" { NOM }
  | "->" { ARROW }
  | "=>" { IMPLY }
  | "forall" { FORALL }
  | id { ID (Lexing.lexeme lexbuf) }
  | white { read lexbuf }
  | '\'' { QUOTE }
  | '^' { XOR }
  | '|' {OR}
  | '(' {LP}
  | ')' {RP}
  | '[' { LB }
  | ']' { RB }
  | '{' { LBB }
  | '}' { RBB }
  | ':' { COLON }
  | '=' { ASSIGN }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }