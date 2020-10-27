/* a parser for extended ldif

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
  open Ldap_ooclient

  let check_attrs attr attrs =
    List.rev_map
      (fun (declared_attr, valu) ->
         if declared_attr = attr then
           valu
         else
           failwith
             ("declared attribute " ^
                "modifies the wrong" ^
                "attribute, " ^
                "attribute: " ^ attr ^
                "declared: " ^
                declared_attr))
      attrs

  let check_empty op attr =
    match op with
        `DELETE -> (op, attr, [])
      | `ADD -> failwith "non sensical empty add"
      | `REPLACE -> failwith "non sensical empty replace"
%}

%token End_of_input Change_type_add Change_type_modrdn
%token Change_type_modify Change_type_delete Dash Newline
%token <string> AttributeType Dn Add Delete Replace
%token <string * string> Attr
%type <Ldap_ooclient.changerec> changerec
%start changerec
%%

operation:
  Add {(`ADD, $1)}
| Delete {(`DELETE, $1)}
| Replace {(`REPLACE, $1)}
;

attrlst:
  Attr attrlst {$1 :: $2}
| Attr {[$1]}

newline:
  Newline {}
| End_of_input {}

modificationterminator:
  Dash newline {}
| newline {}
;

modifications:
  operation attrlst Dash modifications {let (op, attr) = $1 in
                                          (op,
                                           attr,
                                           check_attrs attr $2) :: $4}
| operation Dash modifications {let (op, attr) = $1 in
                                  (check_empty op attr) :: $3}
| operation attrlst modificationterminator {let (op, attr) = $1 in
                                              [(op, attr,
                                                check_attrs attr $2)]}
| operation modificationterminator {let (op, attr) = $1 in
                                      [(check_empty op attr)]}
;

entry:
  Attr entry {let (a, v) = $1 in (a, [v]) :: $2}
| Attr newline {let (a, v) = $1 in [(a, [v])]}

changerec:
  Dn Change_type_modify modifications {`Modification ($1, List.rev $3)}
| Dn Change_type_add entry {let e = new ldapentry in
                              e#set_dn $1;e#add $3;`Addition e}
| Dn Change_type_delete newline {`Delete $1}
| Dn Change_type_modrdn Attr Attr newline {`Modrdn
                                             ($1,
                                              int_of_string (snd $3),
                                              snd $4)}
| End_of_input {raise Ldif_types.Changerec_parser_end}
;
