open Ast_405
open Parsetree

val parse :
  char list ->
  Ast_helper.loc ->
  string ->
  [> `Const of expression
  |  `Fun of int list * (expression -> expression) ] * string option
