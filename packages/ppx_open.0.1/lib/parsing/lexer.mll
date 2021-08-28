{

open Parser
exception Error of string
}

let lower = ['a'-'z' '_']
let upper = ['A'-'Z']

let ident_char = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let lid = lower ident_char*
let uid = upper ident_char*

rule token = parse
    | [' ' '\t']        { token lexbuf }

    | "("               { LPAREN }
    | ")"               { RPAREN }
    | "."               { DOT }
    | ","               { COMMA }

    (* | "exposing"        { EXPOSING } *)
    | "type"            { TYPE }
    | "module"          { MODULE }
    | "as"              { AS }

    | lid               { LOWER_IDENT (Lexing.lexeme lexbuf) }
    | uid               { UPPER_IDENT (Lexing.lexeme lexbuf) }

    | eof               { EOF }
    | _                 { raise (Error ("Lexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }



