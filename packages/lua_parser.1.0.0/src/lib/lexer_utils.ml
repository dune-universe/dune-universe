(*
 The MIT License                                                                                                                                 
                                                                                                                                                 
 Copyright (c) 2020 Jason D. Nielsen <drjdnielsen@gmail.com>
 *)

open Lexing

let pspos lexbuf =
  let p = lexbuf.lex_curr_p in
  Printf.sprintf
    "File: %s, line: %d, col: %d, token: %s"
    p.pos_fname
    p.pos_lnum
    (p.pos_cnum - p.pos_bol)
    (lexeme lexbuf)

let lexeme lexbuf = Lexing.lexeme lexbuf

let new_line lexbuf = Lexing.new_line lexbuf

let len_l = ref 0

let lsbuf = Buffer.create 80

let in_lcomm lexbuf =
  let value = lexeme lexbuf in
  len_l := String.length value - 2
  

let out_lcomm lexbuf longcomm eolcomm =
  let value = lexeme lexbuf in
  let n = String.length value + 1 in
  if n = !len_l then
    eolcomm lexbuf 
  else longcomm lexbuf 


let in_lstr lexbuf =
  Buffer.clear lsbuf;
  let value = lexeme lexbuf in
  Buffer.add_string lsbuf value;
  len_l := String.length value

let out_lstr lexbuf longstr eolstr =
  let value = lexeme lexbuf in
  let n = String.length value + 1 in
  if n = !len_l then
    begin
      Buffer.add_string lsbuf value;
      eolstr lexbuf
    end
  else
    begin
      Buffer.add_string lsbuf value;
      longstr lexbuf
    end

let add_buf chr = Buffer.add_char lsbuf chr

let get_buf () = Buffer.contents lsbuf

let nl_buf () =
  Buffer.add_string lsbuf "\n"
