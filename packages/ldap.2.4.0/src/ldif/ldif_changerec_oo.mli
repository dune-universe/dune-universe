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

(** an exception raised when there is a parse error *)
exception Invalid_changerec of string

(** raised at the end of the change records *)
exception End_of_changerecs

(** Ldif_changerec.iter f change, iterate accross all change entries
    in the specified change object, applying f to each one *)
val iter : ('a -> unit) -> < read_changerec : 'a; .. > -> unit

(** Ldif_changerec.fold f change value, for each change entry en in
    the change object fold computes f (... (f (f value e1) e2) ...) en *)
val fold : ('a -> 'b -> 'a) -> < read_changerec : 'b; .. > -> 'a -> 'a

class change:
  ?in_ch:Pervasives.in_channel ->
  ?out_ch:Pervasives.out_channel ->
  unit ->
object
  method read_changerec: Ldap_ooclient.changerec
  method of_string: string -> Ldap_ooclient.changerec
  method to_string: Ldap_ooclient.changerec -> string
  method write_changerec: Ldap_ooclient.changerec -> unit
end
