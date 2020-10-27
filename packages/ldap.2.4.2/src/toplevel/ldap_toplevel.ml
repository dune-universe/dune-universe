(* Functions which resemble the command line tools, useful in the
   interactive environment

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
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
   USA
*)

open Ldap_ooclient

let eval s =
  let l = Lexing.from_string s in
  let ph = !Toploop.parse_toplevel_phrase l in
  assert(Toploop.execute_phrase false Format.err_formatter ph)
;;

eval "#install_printer Ldap_ooclient.format_entries;;";;
eval "#install_printer Ldap_ooclient.format_entry;;";;
eval "#install_printer Ldap_schemaparser.format_oid;;";;
eval "#install_printer Ldap_schemaparser.format_lcstring;;";;
eval "#install_printer Ldap_schemaparser.format_schema;;";;

let ldap_cmd_harness ~h ~d ~w f =
  let ldap = new ldapcon [h] in
    try
      ldap#bind d ~cred:w;
      let res = f ldap in
        ldap#unbind;
        res
    with exn -> ldap#unbind;raise exn
;;

let ldapsearch ?(s=`SUBTREE) ?(a=[]) ?(b="") ?(d="") ?(w="") ~h filter =
  ldap_cmd_harness ~h ~d ~w
    (fun ldap ->
       ldap#search
         ~base:b ~scope:s
         ~attrs:a filter)
;;

let ldapmodify ~h ~d ~w mods =
  ldap_cmd_harness ~h ~d ~w
    (fun ldap ->
       List.iter
         (fun (dn, ldmod) -> ldap#modify dn ldmod)
         mods)
;;

let ldapadd ~h ~d ~w entries =
  ldap_cmd_harness ~h ~d ~w
    (fun ldap ->
       List.iter
         (fun entry -> ldap#add entry)
         entries)
;;
