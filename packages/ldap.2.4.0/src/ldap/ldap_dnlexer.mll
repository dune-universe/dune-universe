(* lexer for rfc2252 format schemas

   Copyright (C) 2004 Eric Stokes, and The California State University
   at Northridge

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

{
  open Ldap_dnparser
}

let whsp = [ '\t' ' ' ]*
let alpha = [ 'a' - 'z' 'A' - 'Z' ]
let digit = [ '0' - '9' ]
let hexchar = [ '0' - '9' 'A' - 'F' 'a' - 'f' ]
let keychar = [ 'a' - 'z' 'A' - 'Z' '0' - '9' '-' ]
let attributetype = (alpha keychar*) as attribute
let oid = [ '0' - '9' '.' ]+
let special = [ ','  '='  '+'  '<'   '>'  '#'  ';' ]
let quotechar = [^ '\\' '"' ]
let hexpair = hexchar hexchar
let hexstring = hexpair +
let stringchar = [^ '\\' '"' ] # special
let pair = '\\' (special | ' ' | '\\' | '"' | hexpair)
(*
   According to the rfc this is the set of possible values for an attribute value
   We don't implement it directly, instead we split each one into a seperate token
   to make unescaping easier

   string = (stringchar | pair)* | '#' hexstring | '"' (quotechar | pair)* '"'
*)

rule lexdn = parse
    whsp '=' whsp {Equals}
  | whsp '+' whsp {Plus}
  | whsp (',' | ';') whsp {Comma}
  | oid {Oid (Lexing.lexeme lexbuf)}
  | attributetype {AttributeType (Lexing.lexeme lexbuf)}
  | stringchar* ([^ ' '] # special) {String (Lexing.lexeme lexbuf)}
  | (stringchar | pair)* (pair | ([^ ' '] # special)) {StringWithPair (Lexing.lexeme lexbuf)}
  | '#' hexstring {HexString (Lexing.lexeme lexbuf)}
  | '"' (quotechar | pair)* '"' {QuoteString (Lexing.lexeme lexbuf)}
  | eof {End_of_input}
