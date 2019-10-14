(* Functions which resemble the command line tools which many users
   are familar with, useful in the interactive environment

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

(** Functions which resemble the command line tools which many users
    are familar with, useful in the interactive environment *)

(** connect to the specified host and perform a search.
    @param h The ldapurl which names the host and port to connect to
    @param d The dn of the object you with to bind as, default anonymous
    @param w The credentials of the object you wish to bind as, default anonymous
    @param s The scope of the search, default `SUBTREE
    @param b The base of the search
    The final argument is the search filter *)
val ldapsearch :
  ?s:Ldap_types.search_scope ->
  ?a:string list ->
  ?b:string ->
  ?d:string ->
  ?w:string -> h:string -> string -> Ldap_ooclient.ldapentry list

(** connect to the specified host and perform one or more modifications.
    @param h The ldapurl which names the host and port to connect to
    @param d The dn of the object you with to bind as, default anonymous
    @param w The credentials of the object you wish to bind as, default anonymous
    The final argument is a list of (dn, modification) pairs which you want to apply *)
val ldapmodify :
  h:string ->
  d:string ->
  w:string ->
  (string * (Ldap_types.modify_optype * string * string list) list) list ->
  unit

(** connect to the specified host and add the specified objects.
    @param h The ldapurl which names the host and port to connect to
    @param d The dn of the object you with to bind as, default anonymous
    @param w The credentials of the object you wish to bind as, default anonymous
    The final argument is a list of objects you wish to add *)
val ldapadd :
  h:string -> d:string -> w:string -> Ldap_ooclient.ldapentry list -> unit
