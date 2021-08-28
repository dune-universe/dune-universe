open Base
open Lexer

(* 
let show_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  "Line: "
  ^ Int.to_string pos.pos_lnum
  ^ " Position: "
  ^ Int.to_string (pos.pos_cnum - pos.pos_bol + 1) *)

let payload lexbuf =
  let open Result in
  try Ok (Parser.open_payload Lexer.token lexbuf) with
  | Error msg -> Error msg
  | Parser.Error -> Error "syntax error"
