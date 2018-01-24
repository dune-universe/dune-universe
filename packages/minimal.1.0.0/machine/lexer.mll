(*
 * Copyright (c) 2018 Xavier R. Guérin <copyright@applepine.org>
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
 *)

{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let number = '-'? ['0'-'9'] ['0'-'9']*
let symbol = ['@' '<' '>' '?' '!' ':' '=' '*' '/' '-' '+' 'a'-'z' 'A'-'Z' '_'] ['@' '<' '>' '?' '!' ':' '=' '*' '/' '-' '+' 'a'-'z' 'A'-'Z' '0'-'9' '_']*

let comment = '#' [^ '\r' '\n']* newline?

rule read =
  parse
  | comment      { read lexbuf }
  | white        { read lexbuf }
  | newline      { next_line lexbuf; read lexbuf }
  | number       { NUMBER (Int64.of_string (Lexing.lexeme lexbuf)) }
  | 'T'          { TRUE }
  | "NIL"        { NIL }
  | '_'          { ANY }
  | symbol       { SYMBOL (Lexing.lexeme lexbuf) }
  | '''          { QUOTE }
  | '`'          { BACKQUOTE }
  | '~'          { TILDE }
  | '.'          { DOT }
  | '"'          { read_string (Buffer.create 32) lexbuf }
  | '('          { OPEN }
  | ')'          { CLOSE }
  | _            { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof          { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'   ; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'  ; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'  ; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'  ; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'  ; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'  ; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
