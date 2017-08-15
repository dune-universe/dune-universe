(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Channel
open Result

(** Common signatures used in the library. *)

module type CLIENT = sig
  (** A Client allows you to list the disks available on a server, connect to
      a specific disk and then issue read and write requests. *)

  include Mirage_block_lwt.S
    with type error = [ Mirage_block.error | `Protocol_error of Protocol.Error.t ]
     and type write_error = [ Mirage_block.write_error | `Protocol_error of Protocol.Error.t ]

  type size = int64
  (** The size of a remote disk *)

  val list: channel -> (string list, [ `Policy | `Unsupported ]) result Lwt.t
  (** [list channel] returns a list of exports known by the server.
      [`Error `Policy] means the server has this function disabled deliberately.
      [`Error `Unsupported] means the server is old and does not support the query
      function. *)

  val negotiate: channel -> string -> (t * size * Protocol.PerExportFlag.t list) Lwt.t
  (** [negotiate channel export] takes an already-connected channel,
      performs the initial protocol negotiation and connects to
      the named export. Returns [disk * remote disk size * flags] *)
end

module type SERVER = sig
  (** A Server allows you to expose an existing block device to remote clients
      over NBD. *)

  type t
  (** An open connection to an NBD client *)

  type size = int64
  (** The size of a remote disk *)

  type name = string
  (** The name of an export. In the 'new style' protocol as used in nbd >= 2.9.17
      the client must select an export by name. *)

  val connect : channel -> ?offer:name list -> unit -> (name * t) Lwt.t
  (** [connect channel ?offer ()] performs the 'new style' initial handshake
      and options negotiation. If [?offer] is provided then these names will be returned
      if the client requests a list of exports, otherwise we will return EPERM.
      The client's choice of name is returned which must be looked up by the
      application. If the name is invalid, the only option is to close the connection.
      If the name is valid then use the [serve] function. *)

  val serve : t ->  (module Mirage_block_lwt.S with type t = 'b) -> 'b -> unit Lwt.t
  (** [serve t block b] runs forever processing requests from [t], using [block]
      device type [b]. *)

  val close: t -> unit Lwt.t
  (** [close t] shuts down the connection [t] and frees any allocated resources *)
end
