/* a parser for rfc2254 ldap filters

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
*/

%{
  open Ldap_types

  let unhex hex =
    match hex with
        '0' -> 0
      | '1' -> 1
      | '2' -> 2
      | '3' -> 3
      | '4' -> 4
      | '5' -> 5
      | '6' -> 6
      | '7' -> 7
      | '8' -> 8
      | '9' -> 9
      | 'a' -> 10
      | 'b' -> 11
      | 'c' -> 12
      | 'd' -> 13
      | 'e' -> 14
      | 'f' -> 15
      | _ -> failwith "invalid hex digit"

  let unescape_hexpair hex1 hex2 =
    (char_of_int
       ((lor)
          ((lsl) (unhex hex1) 4)
          (unhex hex2)))

  let unescape_stringwithpair s =
    let strm = Stream.of_string s in
    let buf = Buffer.create (String.length s) in
    let rec unescape strm buf =
      try
        match Stream.next strm with
            '\\' ->
              (match Stream.next strm with
                   (',' | '=' | '+' | '<' | '>' | '#' | ';' | '\\' | '"' | ' ') as c ->
                     Buffer.add_char buf c;
                     unescape strm buf
                 | ('0' .. '9' | 'A' .. 'F' | 'a' .. 'f') as hex1 ->
                     let hex2 = Stream.next strm in
                       Buffer.add_char buf (unescape_hexpair hex1 hex2);
                       unescape strm buf
                 | _ -> failwith "invalid escape sequence")
          | c -> Buffer.add_char buf c;unescape strm buf
      with Stream.Failure -> Buffer.contents buf
    in
      unescape strm buf

  let unescape_quotestring s =
    unescape_stringwithpair (String.sub s 1 ((String.length s) - 2))

  let unescape_hexstring s =
    let strm = Stream.of_string s in
    let buf = Buffer.create (String.length s) in
    let rec unescape strm buf =
      try
        let hex1 = Stream.next strm in
        let hex2 = Stream.next strm in
          Buffer.add_char buf (unescape_hexpair hex1 hex2);
          unescape strm buf
      with Stream.Failure -> Buffer.contents buf
    in
      match Stream.next strm with
          '#' -> unescape strm buf
        | _ -> failwith "invalid hexstring"
%}

%token Equals Plus Comma End_of_input
%token <string> AttributeType
%token <string> Oid
%token <string> String
%token <string> StringWithPair
%token <string> HexString
%token <string> QuoteString
%type <Ldap_types.dn> dn
%start dn
%%

attrval:
   AttributeType {$1}
 | Oid {$1}
 | String {$1}
 | StringWithPair {unescape_stringwithpair $1}
 | HexString {unescape_hexstring $1}
 | QuoteString {unescape_quotestring $1}
;

attrname:
   AttributeType {$1}
 | Oid {$1}
;

dn:
  attrname Equals attrval Plus dn
  {match $5 with
       {attr_type=attr_name;attr_vals=vals} :: tl ->
         if $1 = attr_name then
           {attr_type=attr_name;attr_vals=($3 :: vals)} :: tl
         else failwith ("invalid multivalued rdn, expected: " ^ $1)
     | [] -> [{attr_type=$1;attr_vals=[$3]}]}
 | attrname Equals attrval Comma dn {{attr_type=$1;attr_vals=[$3]} :: $5}
 | attrname Equals attrval End_of_input {[{attr_type=$1;attr_vals=[$3]}]}
 | End_of_input {[]}
;
