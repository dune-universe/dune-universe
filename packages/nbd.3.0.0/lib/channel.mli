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

type channel = {
  read:  Cstruct.t -> unit Lwt.t; (** Read a block of data from the channel *)
  write: Cstruct.t -> unit Lwt.t; (** Write a block of data to the channel *)
  close: unit -> unit Lwt.t; (** Close the channel *)
}
(** An open channel to an NBD client or server. *)
