(* a test program for ldap_ooclient

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
open Ldif_oo
open Arg

let () =
  (* stuff to handle command line args *)
  let usg = "testoo -H <ldapurl> -D <dn> -w <pass> -b <base> <filter>" in
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


    (* do the ldap part *)
    if (Array.length Sys.argv) > 9 then
      (parse spec set_filter usg;
       let ldap = new ldapcon [!host] in
       let ldif = new ldif () in
	 ldap#bind  !binddn ~cred: !cred;
	 Ldap_ooclient.iter
	   (fun e -> ldif#write_entry e)
	   (ldap#search_a ~base: !base !filter))
    else
      usage spec usg
