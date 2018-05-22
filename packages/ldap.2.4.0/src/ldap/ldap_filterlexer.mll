(* a lexer for rfc2254 human readable search filters

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
  open Ldap_filterparser
  open Ldap_types

  let star = Pcre.regexp ~study:true "\\*"
  let substr_proto = {substr_initial=[];substr_any=[];substr_final=[]}

  let to_substr v =
    let substrs = Pcre.split ~rex:star v in
      (if v.[0] = '*' then
         (* pcre puts the empty string on the front of the list if the
            delimeter is the first char in the string *)
         let substrs = List.tl substrs in
           if v.[(String.length v) - 1] = '*' then
             {substr_proto with substr_any=substrs}
           else
             {substr_initial=[];
              substr_final=[(List.hd (List.rev substrs))];
              substr_any=(try List.rev (List.tl (List.rev substrs)) with _ -> [])}
       else
         if v.[(String.length v) - 1] = '*' then
           {substr_initial=(try [List.hd substrs] with _ -> []);
            substr_any=(try List.tl substrs with _ -> []);
            substr_final=[]}
         else
           {substr_initial=(try [List.hd substrs] with _ -> []);
            substr_final=(try [List.hd (List.rev substrs)] with _ -> []);
            substr_any=(try (List.rev
                               (List.tl
                                  (List.rev (List.tl substrs))))
                        with _ -> [])})
}

let lparen = '('
let rparen = ')'
let andop = '&'
let orop = '|'
let notop = '!'
let equalop = '='
let colonequalop = ":="
let approxop = '~' equalop
let gteop = '>' equalop
let lteop = '<' equalop
let star = '*'
let attr = [ '0' - '9' 'a' - 'z' 'A' - 'Z' ] +
let hexdigit = [ '0' - '9' 'a' - 'f' 'A' - 'F' ]
let escape = '\\' hexdigit hexdigit
let value = escape | ( [ '\t' ' ' '!' - '~' ] # [ '(' ')' '&' '|' '*' ] )
let values = value +
let colon = ':'
let oid = ( [ '0' - '9' '.' ] + as oid)
let dn = colon "dn"
let matchingrule = colon oid
let extendedmatchattr = (attr as a) matchingrule
let extendeddnattr = (attr as a) dn (matchingrule)?
let substrany = star (values star) +
let substr =
    substrany
  | values substrany
  | substrany values
  | values substrany values
  | values star
  | star values
  | values star values

rule lexfilter = parse
    lparen {LPAREN}
  | rparen {RPAREN}
  | andop {AND}
  | orop {OR}
  | notop {NOT}
  | (attr as a) equalop (substr as v) {ATTREQUALSUB (a, to_substr v)}
  | (attr as a) equalop star {ATTRPRESENT a}
  | (attr as a) equalop (values as v) {ATTREQUAL (a, v)}
  | (attr as a) gteop (values as v) {ATTRGTE (a, v)}
  | (attr as a) lteop (values as v) {ATTRLTE (a, v)}
  | (attr as a) approxop (values as v) {ATTRAPPROX (a, v)}
  | extendedmatchattr colonequalop (values as v) {ATTREXTENDEDMATCH (a, oid, v)}
  | extendeddnattr colonequalop (values as v) {ATTREXTENDEDDN (a, oid, v)}
  | eof {EOF}
