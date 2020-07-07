(*
 * Copyright (c) 2012-2015 Vincent Bernardoff <vb@luminar.eu.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Sedlexing
open Parser

let newline = [%sedlex.regexp? '\n' | '\r' | "\r\n" | "\n\r"]
let tab = [%sedlex.regexp? Chars "\t\x0b"]
let wsp = [%sedlex.regexp? Chars " \t"]

let rec prop_scanner nlines buf lexbuf = match%sedlex lexbuf with
  (* White spaces other than linebreaks are converted to space
     (e.g. no tab, vertical tab, ..). *)
  | tab -> Buffer.add_char buf ' '; prop_scanner nlines buf lexbuf

  (* Soft line break: linebreaks preceded by a \ (soft linebreaks
     are converted to , i.e. they are removed) *)
  | '\\', newline -> prop_scanner (succ nlines) buf lexbuf

  | ']' -> nlines, Buffer.contents buf

  | newline ->
    Buffer.add_string buf (Utf8.lexeme lexbuf);
    prop_scanner (succ nlines) buf lexbuf

  | '\\', Compl tab ->
    Buffer.add_string buf (Utf8.lexeme lexbuf);
    prop_scanner nlines buf lexbuf

  | '\\', tab ->
    Buffer.add_char buf ' ';
    prop_scanner nlines buf lexbuf

  | eof -> failwith "Unterminated prop"

  | any ->
    Buffer.add_string buf (Utf8.lexeme lexbuf);
    prop_scanner nlines buf lexbuf
  | _ -> failwith "prop_scanner"

let rec main_scanner nlines lexbuf =
  let ucase = [%sedlex.regexp? 'A'..'Z'] in
  match%sedlex lexbuf with
  (* Début d'une prop *)
  | '[' ->
    let nlines, content = prop_scanner nlines (Buffer.create 10) lexbuf in
    nlines, PROPCONTENT (content)

  | Plus wsp -> main_scanner nlines lexbuf
  | newline -> main_scanner (succ nlines) lexbuf

  | '(' -> nlines, LPAR
  | ')' -> nlines, RPAR
  | ';' -> nlines, SEMI

  | Plus ucase -> nlines, PROPNAME(Utf8.lexeme lexbuf)
  | eof   -> nlines, EOF
  | _ -> failwith "main_scanner"
