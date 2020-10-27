(* a test program for ldap_funclient

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

(* $Id$ *)

open Ldap_types
open Ldap_funclient
open Arg

let ldif_buffer = Buffer.create 3124
let print_entry e =
  match e with
      `Entry {sr_dn=dn;sr_attributes=attrs} ->
	Buffer.add_string ldif_buffer "dn: ";
	Buffer.add_string ldif_buffer dn;
	Buffer.add_string ldif_buffer "\n";
	List.iter
	  (fun {attr_type=name;attr_vals=vals} ->
	     List.iter
	       (fun aval ->
		  Buffer.add_string ldif_buffer name;
		  Buffer.add_string ldif_buffer ": ";
		  Buffer.add_string ldif_buffer aval;
		  Buffer.add_string ldif_buffer "\n")
	       vals)
	  attrs;
	Buffer.add_string ldif_buffer "\n";
	Buffer.output_buffer stdout ldif_buffer;
	Buffer.clear ldif_buffer
    | `Referral _f -> ()

let main () =
  let usg = "test -H <ldapurl> -D <dn> -w <pass> -b <base> <filter>" in
  let host = ref "" in
  let binddn = ref "" in
  let cred = ref "" in
  let base = ref "" in
  let filter = ref "" in
  let set_host x = host := x in
  let set_binddn x = binddn := x in
  let set_cred x = cred := x in
  let set_base x = base := x in
  let set_filter x = filter := x in
  let spec = [("-H", String(set_host), "host");
	      ("-D", String(set_binddn), "dn to bind with");
	      ("-w", String(set_cred), "password to use when binding");
	      ("-b", String(set_base), "search base")] in
    if (Array.length Sys.argv) > 9 then
      (parse spec set_filter usg;
       let con = init [!host] in
	 bind_s con ~who:!binddn ~cred:!cred;
	 let msgid = search con ~base:!base !filter in
	   try
	     while true
	     do
	       print_entry (get_search_entry con msgid);
	     done
	   with LDAP_Failure (`SUCCESS, _, _) -> print_endline "")
    else
      usage spec usg
;;

main ();;

