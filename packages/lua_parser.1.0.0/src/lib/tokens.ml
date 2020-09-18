(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Sexplib.Conv

type token =
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | CARAT
  | GT
  | LT
  | GE
  | LE
  | EQ
  | NE
  | ASSIGN
  | DOT
  | CAT
  | ELLIPSIS
  | COLON
  | DCOLON
  | SEMI
  | COMMA
  | HASH
  | LB
  | RB
  | LCB
  | RCB
  | LPAR
  | RPAR
  | AND
  | BREAK
  | DO
  | ELSE
  | ELSEIF
  | END
  | FOR
  | FUNCTION
  | GOTO
  | IF
  | IN
  | LOCAL
  | NOT
  | OR
  | REPEAT
  | RETURN
  | THEN
  | UNTIL
  | WHILE
  | NUM_CONST of string
  | IDENT of string
  | BOOL of string
  | STR_CONST of string
  | EOF
[@@deriving sexp, show]
