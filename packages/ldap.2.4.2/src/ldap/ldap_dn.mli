(* Utility functions for operating on dns

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

(** operations on ldap dns *)

(** raised when something goes wrong with conversion to or from a
    string. The integer argument is the charachter which the lexer was
    looking at then the failure ocurred. In the case of to_string the
    integer argument will always be zero. *)
exception Invalid_dn of int * string

(** Given a string representation of a dn, return a structured
    representation.  unescapes any escape sequences present. *)
val of_string : string -> Ldap_types.dn

(** Given a structural representation of a dn, return a string
    representation.  Performs all the necessary escaping to correctly
    represent any structured representation. *)
val to_string : Ldap_types.dn -> string

(** Escape a string which you intend to be part of a VALUE in the
    dn. Do not use on the whole dn, just an attribute value. It is NOT
    necessary to use this if you intend to call to_string on your
    dn. It will be done for you as part of the conversion
    process. This function is exposed for the case where you find it
    easier to manipulate the dn via a regular expression, or other
    string based means, and you find it necessary to escape values. *)
val escape_value : string -> string

(** returns the canonical dn. A simple string compare can tell you
    accurately whether two canonical dns are equal or not. *)
val canonical_dn : string -> string
