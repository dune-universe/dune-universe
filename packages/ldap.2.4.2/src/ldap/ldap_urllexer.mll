(* a quick and dirty rfc 2255 ldap url lexer for referral processing Will
   only parse a subset of the ldapurl

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
  open Ldap_types

  type lexeme = SCHEME
                | COLONSLASHSLASH
                | PORT of string
                | HOST of string
                | DN of string
                | IDENT of string
                | SCOPE of string
                | FILTER of string
                | QUESTION
                | EQUAL
                | CRITICAL
                | SLASH
                | WHSP
                | COMMA

  exception SyntaxError
}

let port = ['0' - '9']+
let host = ['-' '.' '0' - '9' 'a' - 'z' 'A' - 'Z']+
let dn = [',' '=' '0' - '9' 'a' - 'z' 'A' - 'Z']+
let attribute = ['a' - 'z' 'A' - 'Z' '0' - '9']+
let filter = [' ' '(' ')' '&' '|' '!' '~' '=' '>' '<' '.' '\\' '0' - '9' 'a' - 'z' 'A' - 'Z'] +
let scope = "base" | "one" | "sub"

rule lexurl = parse
  | (("ldap" 's'?) as mech) "://" (host as host)? (':' (port as port))? '/'? eof
      {{url_mech=(match mech with "ldap" -> `PLAIN | "ldaps" -> `SSL
                    | _ -> failwith "invalid mechanism") ;
        url_host=host;
        url_port=port;
        url_dn=None;
        url_attributes=None;
        url_scope=None;
        url_filter=None;
        url_ext=None}}
  | _ | eof { raise SyntaxError }

(*
rule lexurl = parse
    "ldap" {SCHEME}
  | "://" {COLONSLASHSLASH}
  | port {PORT (Lexing.lexeme lexbuf)}
  | host {HOST (Lexing.lexeme lexbuf)}
  | dn {DN (Lexing.lexeme lexbuf)}
  | attribute {IDENT (Lexing.lexeme lexbuf)}
  | scope {SCOPE (Lexing.lexeme lexbuf)}
  | filter {FILTER (Lexing.lexeme lexbuf)}
  | ',' {COMMA}
  | '?' {QUESTION}
  | '=' {EQUAL}
  | ':' {COLON}
  | '!' {CRITICAL}
  | '/' {SLASH}
  | ' '* {WHSP}
*)
