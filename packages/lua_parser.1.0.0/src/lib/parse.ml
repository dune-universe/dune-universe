(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Lexer_utils

let parse_with_error lexbuf =
  try Parser.prog Lexer.tok lexbuf
  with Parser.Error -> failwith (pspos lexbuf)

let parse_from_chan chan =
  let lexbuf = Lexing.from_channel chan in
  parse_with_error lexbuf

let parse_from_string str =
  let lexbuf = Lexing.from_string str in
  parse_with_error lexbuf
