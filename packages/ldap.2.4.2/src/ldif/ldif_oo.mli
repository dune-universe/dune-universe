(* create an ldap entry factory from a channel attached to an ldif
   source default is stdin and stdout.

   Copyright (C) 2004 Eric Stokes, Matthew Backes, and The California
   State University at Northridge

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

(** an object oriented interface to the ldif parser *)

(** Ldif_oo.iter f ldif, iterate accross all ldif entries in the
    specified ldif object, applying f to each one *)
val iter : ('a -> unit) -> < read_entry : 'a; .. > -> unit

(** Ldif_oo.fold f ldif value, for each ldif entry en in the ldif
    object fold computes f (... (f (f value e1) e2) ...) en *)
val fold : ('a -> 'b -> 'a) -> < read_entry : 'b; .. > -> 'a -> 'a

(** if you need a fast, low level interface to to_string, this
    function will write ldif directly into a buffer. Setting ext to
    true (defaul false) will write extended ldif. Extended ldif should
    be parsed using the Ldif_changerec_oo module. *)
val entry2ldif : ?ext:bool -> Buffer.t ->
  < attributes : string list; dn : string; get_value : string ->
  string list; .. > -> Buffer.t

(** read all the entries in the named ldif file and return them in a list *)
val read_ldif_file : string -> Ldap_ooclient.ldapentry list

(** write all the entries in the given list to the named file in ldif format *)
val write_ldif_file : string -> Ldap_ooclient.ldapentry list -> unit

class ldif:
  ?in_ch:in_channel ->
  ?out_ch:out_channel ->
  unit ->
object
  method read_entry: Ldap_ooclient.ldapentry
  method of_string: string -> Ldap_ooclient.ldapentry
  method to_string: Ldap_ooclient.ldapentry -> string
  method write_entry: Ldap_ooclient.ldapentry -> unit
end
