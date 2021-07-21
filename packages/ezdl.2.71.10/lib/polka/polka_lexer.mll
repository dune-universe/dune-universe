(*i $Id: polka_lexer.mll,v 1.1.1.1 2002/05/17 16:26:20 bjeannet Exp $ i*)

(* Lexical analysis to convert strings to objects. *)

{
open Polka_parser
open Lexing
exception Error of int * int
}

rule lex = parse
  eof    { TK_EOF }
| [' ' '\t' '\n'] +  { lex lexbuf }

| "V:"   { TK_VERTEX }
| "R:"   { TK_RAY }
| "L:"   { TK_LINE }

(* Opérations arithmétiques *)
| "+"   { TK_PLUS }
| "-"   { TK_MOINS }
| "/"   { TK_DIV }

(* Opérations mixtes *)
| ">"  { TK_SUP }
| "<"   { TK_INF }
| ">="  { TK_SUPEG }
| "<="   { TK_INFEG }
| "="   { TK_EG }

(* Identificateurs *)
| (['0'-'9'])+    { TK_NUM(Big_int.big_int_of_string (lexeme lexbuf)) }
| ['A'-'Z' 'a'-'z' '_']
    ( ['_' 'A'-'Z' 'a'-'z' '0'-'9'] ) *
    ( ['\''] ) *
    { TK_VAR (lexeme lexbuf) }
| _ { raise (Error(lexeme_start lexbuf, lexeme_end lexbuf)) }
