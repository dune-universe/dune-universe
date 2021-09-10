
(* The type of tokens. *)

type token = 
  | XOR
  | STORE
  | SEMICOLON
  | RP
  | RBB
  | RB
  | QUOTE
  | OR
  | NOM
  | LP
  | LBB
  | LB
  | IMPLY
  | ID of (string)
  | FORALL
  | EOF
  | DECIMAL of (int)
  | COMMA
  | COLON
  | ASSIGN
  | ARROW

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Builder.builder list)
