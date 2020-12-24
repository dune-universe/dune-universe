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

type tls_channel = {
  read_tls: Cstruct.t -> unit Lwt.t;
  write_tls: Cstruct.t -> unit Lwt.t;
  close_tls: unit -> unit Lwt.t;
}

type cleartext_channel = {
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

let generic_of_tls_channel ch = {
  read = ch.read_tls;
  write = ch.write_tls;
  close = ch.close_tls;
  is_tls = true;
}

let generic_of_cleartext_channel ch = {
  read = ch.read_clear;
  write = ch.write_clear;
  close = ch.close_clear;
  is_tls = false;
}
