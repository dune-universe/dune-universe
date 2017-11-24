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
  open Bmd_parser

  let keyword_table =
    let tbl = Hashtbl.create 37 in
    begin
      List.iter (fun (key, data) -> Hashtbl.add tbl key data)
	[ (* natural *)
	  "a", A;
	  "an", A;
	  "is", IS;
	  "can", CAN;
	  "be", BE;
	  "has", HAS;
	  "one", ONE;
	  "of", OF;
	  "identified", IDENTIFIED;
	  "by", BY;
	  "concept", CONCEPT;
	  "time-stamped", TIMESTAMPED;
	  "related", RELATED;
	  "to", TO;
	  "with", WITH;
	  (* types *)
	  "integer", INTEGER;
	  "date", DATE;
	  "time", TIME;
	]; tbl
    end

}

let newline = ('\010' | '\013' | "\013\010")
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '-' '_' '\'' '0'-'9']

rule token sbuff = parse
| eof { EOF }
| "&" { AMP }
| "." { DOT }
| ":" { COLON }
| "," { COMMA }
| "(" { LPAREN }
| ")" { RPAREN }
| [' ' '\t']
    { token sbuff lexbuf }
| newline
    { Lexing.new_line lexbuf; token sbuff lexbuf }
| letter identchar*
    { let s = Lexing.lexeme lexbuf in
      try Hashtbl.find keyword_table s
      with Not_found -> IDENT s }
| _
    { raise (LexError ("At offset "^
                       (string_of_int (Lexing.lexeme_start lexbuf))^
                       ": unexpected character.\n" )) }

