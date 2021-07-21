(*i $Id: polka_lexer.mli,v 1.1.1.1 2002/05/17 16:26:20 bjeannet Exp $ i*)

(* Lexical analysis for interactive toplevel. *)

exception Error of int * int
val lex: Lexing.lexbuf -> Polka_parser.token 
