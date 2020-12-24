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

(** Channels represent connections between clients and servers. *)

(** An open channel to an NBD client or server. *)

type tls_channel = {
  read_tls: Cstruct.t -> unit Lwt.t;
  write_tls: Cstruct.t -> unit Lwt.t;
  close_tls: unit -> unit Lwt.t;
}

type cleartext_channel =  {
  read_clear: Cstruct.t -> unit Lwt.t;
  write_clear: Cstruct.t -> unit Lwt.t;
  close_clear: unit -> unit Lwt.t;

  make_tls_channel: (unit -> tls_channel Lwt.t) option;
}

type generic_channel = {
  is_tls: bool;
  read: Cstruct.t -> unit Lwt.t;
  write: Cstruct.t -> unit Lwt.t;
  close: unit -> unit Lwt.t;
}

type channel = generic_channel

val generic_of_tls_channel: tls_channel -> generic_channel

val generic_of_cleartext_channel: cleartext_channel -> generic_channel
