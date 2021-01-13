(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2020 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

{ (* BEGIN HEADER *)

  
open Lexing
open Parser

} (* END HEADER *)


let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ]

let digit = [ '0'-'9' ]

let positive = ([ '1'-'9'] digit*)

let number = (digit | positive | '-' positive)
                           
let letter = [ 'A'-'Z' 'a'-'z' ]

let dollar = '$'

let plain_id = (dollar | '_')? letter (letter | digit | '_' | '#')*

let idx_id = plain_id dollar number

let pragma = "##" plain_id

let comment_line = ("--")

let reserved_symbol = [ '$' '%' '\\' '`' '@' ]

let builtin_iop = ( "add" | "neg" | "minus" | "card" )
                  
                 
rule main infile = parse
| reserved_symbol as c
  { Msg.Fatal.lexical
    @@ fun args -> args infile lexbuf (Printf.sprintf "reserved character: '%c'" c)}
| newline
    { new_line lexbuf; main infile lexbuf }
| whitespace+
    { main infile lexbuf }
| number as i
    {
      try
        (NUMBER (int_of_string i))
      with Failure _ ->
        Msg.Fatal.lexical
        @@ fun args -> args infile lexbuf ("invalid integer constant '" ^ i ^ "'")
    }   
| "run"
    { RUN }
| "expect"
    { EXPECT }
| "sat"
    { SAT }
| "unsat"
    { UNSAT }
| "invariant"
    { INVARIANT }
(*| "int"
      { INT }*)
| "true"
		{ TRUE }
| "false"
		{ FALSE }
                    (* FUTURE OPERATORS *)
| "after"
    { AFTER }
| "always"
    { ALWAYS }
| "eventually"
    { EVENTUALLY }
| "until"
    { UNTIL }
| "releases"
    { RELEASES }
                    (* PAST OPERATORS *)
| "before"
    { BEFORE }
| "historically"
    { HISTORICALLY }
| "once"
    { ONCE }
| "since"
    { SINCE }
| "triggered"
    { TRIGGERED }
                    (* F-O QUANTIFIERS + MULTIPLICITIES *)
| "all"
    { ALL }
| "some"
    { SOME }
| "one"
    { ONE }
(*| "set"
    { SET }*)
| "no"
    { NO }
| "lone"
    { LONE }
| "let"
    { LET }
| "disj"
    { DISJ }
| ("iff")
    { IFF }
| ("implies")
    { IMPLIES }
| "then"
  { THEN }
| "else"
    { ELSE }
| ("or")
    { OR }
| ("and")
    { AND }
| "in"
    { IN }
| ("not" | "!") (whitespace | newline)+ "in" (* TODO: take comments into account *)
    { NOT_IN}
| "inst"
    { (* INST *)
Msg.Fatal.lexical
@@ fun args -> args infile lexbuf ("`inst` not implemented (yet?)")
    }
| "sym"
    { SYM }
| ("not")
    { NOT }

| "var"
    { VAR }
| "const"
    { CONST }
| "univ"
    { UNIV }
| "iden"
    { IDEN }
| "none"
    { NONE }
| idx_id as id
  { (IDX_ID id) }
| plain_id as id
  { (PLAIN_ID id) }
| ":" (positive as ca)
  { (COLON_ARITY (int_of_string ca)) }
| "#"
  { HASH }
| "!="
    { NEQ }
| "!"
    { NOT }
| "'"
    { PRIME }
| ";"
    { SEMI }
| "&"
    { INTER }
| "->"
    { ARROW }
| "<:"
    { LPROJ }
| ":>"
    { RPROJ }
| ".."
		{ DOTDOT }
| "."
    { DOT }
| "~"
    { TILDE }
| "*"
    { STAR }
| "^"
    { HAT } 
| "="
    { EQ }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "["
    { LBRACKET }
| "]"
    { RBRACKET }
| ","
    { COMMA }
| ":"
    { COLON }
| "{"
    {  LBRACE }
| "}"
    {  RBRACE }
| "++"
    { OVERRIDE }
| "+"
    { PLUS }
| "-"
    { MINUS }
| ("=<" | "<=")
    { LTE }
| ">="
    { GTE }
| "<"
    { LT }
| ">"
    { GT }
| "&&"
    { AND }
| "||"
    { OR }
| "=>"
    { IMPLIES }
| "<=>"
    { IFF }
| "|"
    { BAR }
| comment_line (* [^newline]* newline *)
    { line_comment infile lexbuf }
    (* new_line lexbuf; main tokens lexbuf } *)
(* | comment_line eof *)
(*     { tokenize EOF lexbuf :: tokens } *)
| "(*"
    { comment infile 1 lexbuf }
    (* { comment (lexeme_start_p lexbuf) tokens lexbuf; *)
    (*   main tokens lexbuf } *)
 | eof 
     { EOF } 
| _ as c
    { Msg.Fatal.lexical
@@ fun args -> args infile lexbuf ("unexpected character(s): " 
                    ^ (String.make 1 c)) }
(* and comment openingp tokens = parse *)

and line_comment infile = parse
| newline
    { new_line lexbuf; main infile lexbuf }
| eof  
     { EOF } 
| _
    { line_comment infile lexbuf }
  

and comment infile opened = parse
| "(*"
    { comment infile (opened + 1) lexbuf }
| "*)"
    { let nb = opened - 1 in
      if nb < 1 then main infile lexbuf
      else comment infile nb lexbuf }
| newline
    { new_line lexbuf; comment infile opened lexbuf }
| eof
    { Msg.Fatal.lexical
@@ fun args -> args infile lexbuf "end of file within unterminated comment" }
| _
    { comment infile opened lexbuf }


{ (* BEGIN FOOTER *)

  

} (* END FOOTER *)
