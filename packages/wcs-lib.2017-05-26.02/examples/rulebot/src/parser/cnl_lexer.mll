(*
 *  This file is part of the Watson Conversation Service OCaml API project.
 *
 * Copyright 2016-2017 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

{
  open Lexer_util
  open Cnl_parser

  let keyword_table =
    let tbl = Hashtbl.create 37 in
    begin
      List.iter (fun (key, data) -> Hashtbl.add tbl key data)
	[ (* rule *)
	  "when", WHEN;
	  "if", IF;
	  "then", THEN;
	  "occurs", OCCURS;
	  "called", CALLED;
	  (* actions *)
	  "print", PRINT;
	  "define", DEFINE;
	  "emit", EMIT;
	  "set", SET;
	  "as", AS;
	  "to", TO;
	  (* literals *)
	  "true", TRUE;
	  "false", FALSE;
	  (* expressions *)
	  "is", IS;
	  "less", LESS;
	  "more", MORE;
	  "than", THAN;
	  "over", OVER;
	  "under", UNDER;
	  "not", NOT;
	  "and", AND;
	  "or", OR;
	  (* this expression *)
	  "this", THIS;
	  (* new expression *)
	  "new", NEW;
	  "where", WHERE;
	  (* natural *)
	  "a", A;
	  "the", THE;
	  "of", OF;
	]; tbl
    end

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '\'' '0'-'9']

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* (frac exp? | exp)

rule token sbuff = parse
| eof { EOF }
| "+" { PLUS }
| "-" { MINUS }
| "*" { MULT }
| "/" { DIV }
| ";" { SEMI }
| "," { COMMA }
| "(" { LPAREN }
| ")" { RPAREN }
| "_" { UNDER }
| [' ' '\t']
    { token sbuff lexbuf }
| newline
    { Lexing.new_line lexbuf; token sbuff lexbuf }
| float as f
    { FLOAT (float_of_string f) }
| ('-'? ['0'-'9']+) as i
    { INT (int_of_string i) }
| '"'
    { reset_string sbuff; string sbuff lexbuf }
| '''
    { reset_string sbuff; quote_string sbuff lexbuf }
| letter identchar*
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s }
| _
    { raise (LexError ("At offset "^(string_of_int (Lexing.lexeme_start lexbuf))^": unexpected character.\n")) }

and string sbuff = parse
  | "\"\"" { add_char_to_string sbuff '"'; string sbuff lexbuf }                         (* Escaped quote *)
  | "\013\n" { add_char_to_string sbuff '\n'; string sbuff lexbuf }
  | "\013" { add_char_to_string sbuff '\n'; string sbuff lexbuf }
  | '"'    { let s = get_string sbuff in STRING s }  (* End of string *)
  | eof    { raise (LexError "String not terminated.\n") }
  | _      { add_char_to_string sbuff (Lexing.lexeme_char lexbuf 0); string sbuff lexbuf }

and quote_string sbuff = parse
  | "''" { add_char_to_string sbuff '\''; quote_string sbuff lexbuf }                         (* Escaped quote *)
  | "\013\n" { add_char_to_string sbuff '\n'; quote_string sbuff lexbuf }
  | "\013" { add_char_to_string sbuff '\n'; quote_string sbuff lexbuf }
  | '''    { let s = get_string sbuff in QSTRING s }  (* End of string *)
  | eof    { raise (LexError "String not terminated.\n") }
  | _      { add_char_to_string sbuff (Lexing.lexeme_char lexbuf 0); quote_string sbuff lexbuf }


