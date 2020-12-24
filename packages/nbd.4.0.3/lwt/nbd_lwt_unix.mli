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

(** Network Block Device client and servers for Unix *)

open Nbd

type tls_role =
  | TlsClient of Ssl.context
  | TlsServer of Ssl.context

val connect: string -> int -> Channel.channel Lwt.t
(** [connect hostname port] connects to host:port and returns
    a [generic_channel] with no TLS ability or potential. *)

val cleartext_channel_of_fd: Lwt_unix.file_descr -> tls_role option -> Channel.cleartext_channel
(** [cleartext_channel_of_fd fd role] returns a channel from an existing file descriptor.
    The channel will have a [make_tls_channel] value that corresponds to [role]. *)

val init_tls_get_ctx: ?curve:string -> certfile:string -> ciphersuites:string -> Ssl.context
(** Initialise the Ssl (TLS) library and then create and return a new context. *)

val with_block: string -> (Block.t -> 'a Block.io) -> 'a Block.io
(** [with_block filename f] calls [Block.connect filename] and applies [f] to the result,
    with a guarantee to call [Block.disconnect] afterwards. *)

val with_channel:
  Lwt_unix.file_descr ->
  tls_role option ->
  (Nbd.Channel.cleartext_channel -> 'a Block.io) ->
  'a Block.io
(** [with_channel fd role f] calls [cleartext_channel_of_fd fd role] then
    applies [f] to the resulting channel, with a guarantee to call
    the channel's [close_clear] function afterwards. *)

module Client: S.CLIENT
(** A client allows you to access remote disks *)

module Server: S.SERVER
(** A server allows you to expose disks to remote clients *)
