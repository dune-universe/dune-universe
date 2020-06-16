{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

let int     = '-'? ['0'-'9'] ['0'-'9']*
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id      = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let size    = ['0'-'9'] ['0'-'9']*
let hex     = size "'h" ['0'-'9' 'a'-'f' 'A'-'F']+
let dec     = size "'d" ['0'-'9']+
let bin     = size "'b" ['0'-'1']+
let oct     = size "'o" ['0'-'7']+

rule read =
  parse
  | white        { read lexbuf }
  | newline      { next_line lexbuf; read lexbuf }
  | int          { L_INT (int_of_string (Lexing.lexeme lexbuf)) }
  | hex          { L_HEX (Lexing.lexeme lexbuf) }
  | dec          { L_DEC (Lexing.lexeme lexbuf) }
  | bin          { L_BIN (Lexing.lexeme lexbuf) }
  | oct          { L_OCT (Lexing.lexeme lexbuf) }
  | '"'          { read_string (Buffer.create 17) lexbuf }
  | "package"    { PACKAGE }
  | "endpackage" { ENDPACKAGE }
  | "localparam" { LOCALPARAM }
  | "bit"        { BIT }
  | "logic"      { LOGIC }
  | "reg"        { REG }
  | "byte"       { BYTE }
  | "shortint"   { SHORTINT }
  | "int"        { INT }
  | "longint"    { LONGINT }
  | "integer"    { INTEGER }
  | "time"       { TIME }
  | "int"        { INT }
  | id           { ID (Lexing.lexeme lexbuf) }
  | '='          { EQUAL }
  | ';'          { SEMICOLON }
  | ','          { COMMA }
  | _            { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof          { EOF }

and read_string buf =
  parse
  | '"'       { L_STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
