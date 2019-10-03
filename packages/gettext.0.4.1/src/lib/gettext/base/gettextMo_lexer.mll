(**************************************************************************)
(*  ocaml-gettext: a library to translate messages                        *)
(*                                                                        *)
(*  Copyright (C) 2003-2008 Sylvain Le Gall <sylvain@le-gall.net>         *)
(*                                                                        *)
(*  This library is free software; you can redistribute it and/or         *)
(*  modify it under the terms of the GNU Lesser General Public            *)
(*  License as published by the Free Software Foundation; either          *)
(*  version 2.1 of the License, or (at your option) any later version;    *)
(*  with the OCaml static compilation exception.                          *)
(*                                                                        *)
(*  This library is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Lesser General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Lesser General Public      *)
(*  License along with this library; if not, write to the Free Software   *)
(*  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *)
(*  USA                                                                   *)
(**************************************************************************)

{

open GettextMo_parser;;

}

rule
token_field_name = parse
  "Content-Type" [' ''\t']* ':'      { CONTENT_TYPE(token_field_value lexbuf) }
| "Plural-Forms" [' ''\t']* ':'      { PLURAL_FORMS(token_field_value lexbuf) }
| ([^'\n''\r''\t'' ']+ as id) [' ''\t']* ':' { FIELD_NAME(id, token_field_value lexbuf) }
| eof                                { EOF }
| _                                  { token_field_name lexbuf}
and
token_field_value = parse
  [^'\n''\r']* as str           { str }
and
token_field_plural_value = parse
  "nplurals"                    { NPLURALS }
| ';'                           { SEMICOLON }
| "plural"                      { PLURAL }
|  "?"                          { QUESTION_MARK }
| ":"                           { COLON }
| "||"                          { OR }
| "&&"                          { AND }
| "=="                          { EQ }
| '='                           { EQUAL }
| "!="                          { NEQ }
| "<="                          { LE }
| "<"                           { L }
| ">="                          { GE }
| ">"                           { G }
| "+"                           { PLUS }
| "-"                           { MINUS }
| "*"                           { MUL }
| "/"                           { DIV }
| "%"                           { MOD }
| "!"                           { NOT }
| '('                           { LPAREN }
| ')'                           { RPAREN }
| "n"                           { ID }
| ['0'-'9']+ as nbr             { (NUMBER (int_of_string nbr) ) }
| eof                           { EOF }
| [' ''\t']                     { token_field_plural_value lexbuf }
and
token_field_content_type = parse
  "charset"                     { CHARSET }
| ';'                           { SEMICOLON }
| '='                           { EQUAL }
| [^' ''\t'';''=']+ as str      { (STRING str) }
| [' ''\t']                     { token_field_content_type lexbuf }
| eof                           { EOF }
