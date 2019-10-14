(*
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

(** A functional ldap server construction kit *)

open Ldap_types

(** raised whenever an error occurrs in the server *)
exception Server_error of string

type connection_id = int

(** This structure is the guts of the ldap server. For each operation that you
    implement put the function (or closure) of the correct type in this
    structure. Any functions you set as None will return
    `UNWILLING_TO_PERFORM, with the error string set to "not implemented".
    bi_init will be called (if it is provided) before the server is brought
    up, and bi_close (if it is provided) will be called before the server is
    brought down. This interface is based loosely on the back-end api in
    OpenLDAP.*)
type backendInfo = {
  bi_op_bind : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_unbind : (connection_id -> ldap_message -> unit) option;
  bi_op_search : (connection_id -> ldap_message -> (unit -> ldap_message)) option;
  bi_op_compare : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modify : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_modrdn : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_add : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_delete : (connection_id -> ldap_message -> ldap_message) option;
  bi_op_abandon : (connection_id -> ldap_message -> unit) option;
  bi_op_extended : (connection_id -> ldap_message -> ldap_message) option;
  bi_init : (unit -> unit) option;
  bi_close : (unit -> unit) option;
}

type log_level =
    [ `GENERAL
    | `CONNECTION
    | `OPERATIONS
    | `ERROR
    | `TRACE ]

(** This abstract type contains the server context. It has the listening,
    socket, all the connected client sockets, and some internal data
    structures. *)

type server_info

(** Initialize the server, create the listening socket and return the
    server context, which you will pass to serv to process
    connections. log is a string -> log_level -> unit function to which log
    messages will be sent. *)
val init : ?log:(log_level -> string -> unit) -> ?port:int -> backendInfo -> server_info

(** Shutdown the server *)
val shutdown : server_info -> unit

(** Using the supplied server context, begin processing ldap operations. This
    function should never terminate unless there is an exceptional condition, in
    which case the exception will be raised. In many cases it is safe to restart
    the server process when an exception happens. *)
val run : server_info -> unit
