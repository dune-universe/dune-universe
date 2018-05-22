(* lexer for extended ldif

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
  open Ldif_changerec_parser
  open Netencoding
}

let nl = '\n'
let whsp = ' ' *
let mustsp = ' ' +
let alphanum = ['0' - '9' 'a' - 'z' 'A' - 'Z']
let anyprintablechar = ['\t' ' ' - '~']
let attrname =  alphanum +
let attrval = (anyprintablechar | '\n' ' ') +

rule lexcr = parse
  | "dn:" mustsp ([' ' - '~']+ as dn) nl {Dn dn}
  | "changetype:" mustsp "modify" nl {Change_type_modify}
  | "changetype:" mustsp "delete" nl {Change_type_delete}
  | "changetype:" mustsp "modrdn" nl {Change_type_modrdn}
  | "changetype:" mustsp "add" nl {Change_type_add}
  | "add:" mustsp (attrname as name) nl {Add name}
  | "delete:" mustsp (attrname as name) nl {Delete name}
  | "replace:" mustsp (attrname as name) nl {Replace name}
  | (attrname as attr) ':' mustsp (attrval as valu) nl {Attr (attr, valu)}
  | (attrname as attr) "::" mustsp (attrval as valu) nl {Attr (attr, Base64.decode valu)}
  | '-' nl {Dash}
  | nl + {Newline}
  | eof {End_of_input}
