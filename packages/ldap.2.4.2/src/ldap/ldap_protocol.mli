(* an implementation of the ldap wire protocol

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

(** an implementation of the ldap wire protocol *)

open Ldap_types
open Lber

(** return the int asociated with the specified result code *)
val encode_resultcode : ldap_resultcode -> int

(** return the result code for the specified int, error codes which do
    not map to a code defined within the standard (or any of our own
    internal ones) will be represented as (`UNKNOWN_ERROR of int), where
    int is the unknown error code. *)
val decode_resultcode : int -> ldap_resultcode

(** encode a value of type ldap_message using lber and return
    a string which is ready to be put on the wire *)
val encode_ldapmessage : ldap_message -> string

(** decode an ldap_message from the wire, and build/return a
  structure of type ldap_message *)
val decode_ldapmessage : readbyte -> ldap_message
