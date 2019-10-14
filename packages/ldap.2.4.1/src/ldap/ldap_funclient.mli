(* a functional interface to ldap

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

(** a functional ldap client interface *)

open Unix
open Ldap_types
open Lber

type msgid
type conn
type modattr = modify_optype * string * string list
type result = Ldap_types.search_result_entry list
type entry = Ldap_types.search_result_entry
type authmethod = [ `SIMPLE | `SASL ]
type search_result =
  [ `Entry of entry
  | `Referral of string list
  | `Success of (ldap_controls option) ]
type page_control =
  [ `Noctrl
  | `Initctrl of int
  | `Subctrl of (int * string) ]
(** Initializes the conn data structure, and opens a connection to the
  server.  init
  [["ldap://rrhost.example.com/";"ldap://backup.example.com:1389"]].
  init is round robin dns aware, if dns returns multiple mappings it
  will try each one before finially failing. It also takes a list of
  hostnames, so you can specify backup servers to try. SSL and TLS are
  supported if selected at compile time.

  @param version the protocol version to use to
  connect, default is version 3. And actually, version 2 will probably
  not work correctly without some tweaking.

  @raise LDAP_Failure any
  failure to connect to the server will result in LDAP_Failure with
  the result_code set to `LOCAL_ERROR.

  @raise Failure May raise
  Failure "int_of_string" if you pass it a malformed url. May also
  raise various lexer errors under the same conditions. *)
val init : ?connect_timeout:int -> ?version:int -> string list -> conn

(** close the connection to the server. You may not use the conn
  after you have unbound, if you do you will get an exception. *)
val unbind : conn -> unit

(** authenticatite to the server. In this version only simple binds
  are supported, however the ldap_protocol.ml module DOES implement
  sasl binds. It would be fairly easy to support them here. We
  eventually will.

  @param who the dn to bind as
  @param cred the credentials to authenticate with. For `SIMPLE binds
  this is a password, but for `SASL binds it can be nearly
  anything. Perhaps a hash of the thumb print of your first born is
  sufficent.
  @param auth_method either `SIMPLE (the default) or `SASL

  @raise LDAP_Failure for bind errors such as `INVALID_CREDENTIALS
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val bind_s :
  ?who:string -> ?cred:string -> ?auth_method:[> `SIMPLE ] -> conn -> unit

(** Search for the given entry with the specified base node and search
  scope, optionally limiting the returned attributes to those listed in
  'attrs'. aliasderef sets the server's alias dereferencing policy,
  sizelimit is the number of entries to return, timelimit is the number
  of seconds to allow the search to run for, attrsonly tells the server
  not to return the values. This is the asyncronus version of search
  (it does not block) you will need to call the get_search_entry
  function below to actually get any data back. This function will
  return a msgid which you must use when you call get_search_entry.

  @param base The dn of the object in the tree to use as the base
  object, the search will only cover children of this object, and will
  be further governed by scope.
  @param scope The depth in the tree to look for the requested
  object. There are three possible values, `BASE, `ONELEVEL, and
  `SUBTREE. `BASE means to only search the base object, the search
  will return exactly 1 or 0 objects. `ONELEVEL means to search one
  level under the base, only immediate children of the base object
  will be considered. `SUBTREE means to search the entire tree under
  the base object.
  @param aliasderef Controls when aliases are dereferenced.
  @param sizelimit The maximum number of objects to return
  @param timelimit The maximum time, in seconds, that the search will
  be allowed to run before terminateing.
  @param attrs The list of attribute types (names) to include [[]]
  (the default) means all.
  @param attrsonly return only attribute types (names), not any of the
  values

  @raise LDAP_Failure for immediate errors (bad filter, etc)
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val search :
  ?base:string ->
  ?scope:Ldap_types.search_scope ->
  ?aliasderef:Ldap_types.alias_deref ->
  ?sizelimit:int32 ->
  ?timelimit:int32 ->
  ?attrs:string list ->
  ?attrsonly:bool ->
  ?page_control:page_control -> conn -> string -> msgid

(** fetch a search entry from the wire using the given msgid. The
  entry could be a search entry, OR it could be a referral structure.

  @raise LDAP_Failure for all results other than `SUCCESS (except referrals)
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val get_search_entry :
  conn ->
  msgid ->
  [> `Entry of Ldap_types.search_result_entry | `Referral of string list ]

(** fetch a search entry from the wire using the given msgid. The
  entry could be a search entry, OR it could be a referral structure.

  The version supports passing ldap_controls (like page control) through on
  success. Returning an entry of type `SUCCESS was thus needed.

  @raise LDAP_Failure for all results other than `SUCCESS (except referrals)
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val get_search_entry_with_controls :
  conn ->
  msgid ->
  [> `Entry of Ldap_types.search_result_entry |
     `Referral of string list |
     `Success of (ldap_controls option) ]

(** abandon the async request attached to msgid.

  @raise Encoding_error for encoder errors (unlikely, probably a bug) *)
val abandon : conn -> msgid -> unit

(** This is the syncronus version of search. It blocks until the
  search is complete, and returns a list of objects. It is exactly the
  same in all other ways. *)
val search_s :
  ?base:string ->
  ?scope:Ldap_types.search_scope ->
  ?aliasderef:Ldap_types.alias_deref ->
  ?sizelimit:int32 ->
  ?timelimit:int32 ->
  ?attrs:string list ->
  ?attrsonly:bool ->
  conn ->
  string ->
  [> `Entry of Ldap_types.search_result_entry | `Referral of string list ]
  list

(** add entry to the directory

  @raise LDAP_Failure for all results other than `SUCCESS
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val add_s : conn -> entry -> unit

(** delete the entry named by dn from the directory

  @raise LDAP_Failure for all results other than `SUCCESS
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val delete_s : conn -> dn:string -> unit

(** apply the list of modifications to the named entry

  @param dn The dn of the object to modify
  @param mods The list of modifications to apply

  @raise LDAP_Failure for all results other than `SUCCESS
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val modify_s :
    conn ->
    dn:string ->
    mods:(Ldap_types.modify_optype * string * string list) list -> unit

(** change the rdn, and optionally the superior entry of dn

  @param deleteoldrdn Delete the old rdn value, (default true)
  @param newsup The new superior dn of the object (default None)
  @param dn The dn of the object to modify
  @param newrdn The new rdn value (eg. cn=bob)

  @raise LDAP_Failure for all results other than `SUCCESS
  @raise Decoding_error for decoder errors (unlikely, probably a bug)
  @raise Encoding_error for encoder errors (unlikely, probably a bug)
*)
val modrdn_s :
  ?deleteoldrdn:bool ->
  ?newsup:'a option -> conn -> dn:string -> newdn:string -> unit
