open Rets_parser
open Printf
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let run_parser lexbuf =
  try
    prog Rets_lexer.read lexbuf
  with
  | Rets_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    exit (-1)
  | Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)