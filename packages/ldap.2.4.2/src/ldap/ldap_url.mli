(* a quick and dirty rfc 2255 ldap url lexer for referral processing Will
   only parse a subset of the ldapurl

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
*)

(** a library for parsing a subset of the ldapurl syntax *)

(** will be raised in the event of a parse or type error. The integer
    is the location of the error, measured in charachters from the
    left, and the string is a description of the error. The current
    lexer does not correctly set the charachter location, however
    future lexers will. *)
exception Invalid_ldap_url of int * string

(** internalize the url contained in the string argument *)
val of_string : string -> Ldap_types.ldap_url
