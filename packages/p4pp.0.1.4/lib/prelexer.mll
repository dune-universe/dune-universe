(* Copyright 2019-present Cornell University
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy
 * of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software 
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT 
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the 
 * License for the specific language governing permissions and limitations 
 * under the License. 
 *)
{
open Lexing
exception Error of string

let buf = Buffer.create 10_000 

let filename = ref ""
let emit_line = ref 0
let real_line = ref 0

let reset fn =
  filename := fn;
  emit_line := 1;
  real_line := 1;                 
  Buffer.reset buf
  
let emit s = 
  Buffer.add_string buf s

let emit_newline () = 
  incr (emit_line);
  incr (real_line);
  Buffer.add_string buf "\n";
  if !real_line <> !emit_line then 
    begin
      emit_line := !real_line;
      Buffer.add_string buf 
        (Printf.sprintf "#line %d \"%s\"\n"
           (!real_line)
           (!filename))
    end
  
let hide_newline () = 
  incr real_line

}

let whitespace = [' ' '\t' '\r']*

rule lex = parse
  | "/*"
      { multiline_comment lexbuf; lex lexbuf }
  | "//"
      { singleline_comment lexbuf; emit_newline (); lex lexbuf }
  | "\""
      { emit (lexeme lexbuf); string lexbuf }
  | "\\\n" 
      { lex lexbuf}
  | "\n"
      { emit_newline (); lex lexbuf }
  | eof
      { Buffer.contents buf }
  | _
      { emit (lexeme lexbuf); lex lexbuf }
  
and string = parse
  | eof
    { raise (Error "EOF in string literal" ) }
  | "\n"
    { raise (Error "newline in string literal") } 
  | "\\\""
    { emit (lexeme lexbuf); string lexbuf }
  | '"'
    { emit (lexeme lexbuf); lex lexbuf }
  | _
    { emit (lexeme lexbuf); string lexbuf }

(* Multi-line comment terminated by "*/" *)
and multiline_comment = parse
  | "*/"   { emit " " }
  | "\n"   { hide_newline (); multiline_comment lexbuf }
  | _      { multiline_comment lexbuf }
  | eof    { failwith "unterminated comment" }
      
(* Single-line comment terminated by a newline *)
and singleline_comment = parse
  | "\n"   { () }
  | eof    { () }
  | _      { singleline_comment lexbuf }
                 
{

}
