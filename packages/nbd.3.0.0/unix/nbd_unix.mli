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

type error = int32
(** An error code returned from the server *)

type handle = int64
(** A client chosen request identifier used to match up responses
    with requests. *)

type size = int64
(** The size of the remote disk *)

val connect : string -> int -> Unix.file_descr * size * Nbd.flag list
(** [connect hostname port] connects to an NBD server and performs
    the initial protocol negotiation. Returns
    (connected Unix.file_descr * remote disk size * flags) *)

val negotiate : Unix.file_descr -> size * Nbd.flag list
(** [negotiate fd] takes an already-connected Unix.file_descr and
    performs the initial protocol negotiation. Returns
    (remote disk size * flags) *)

val read : Unix.file_descr -> int64 -> int32 -> string option
(** [read fd offset length] attempts to read [length] bytes starting
    at [offset]. If successful returns [Some data], otherwise returns
    None *)

val write : Unix.file_descr -> int64 -> string -> int -> int -> error option
(** [write fd dst_offset buf offset len] synchronously writes [len]
    bytes from [buf] at [offset] to the remote disk at offset [dst_offset],
    returning None on success and (Some error) on failure. *)

val write_async : Unix.file_descr -> int64 -> string -> int -> int -> handle -> unit
(** [write fd dst_offset buf offset length handle] writes [length]
    bytes from [buf] starting at [offset] to the remote disk at
    [dst_offset], tagging the request with identifier [handle]. *)

val write_wait : Unix.file_descr -> handle * error option
(** [write_wait fd] reads a write reply, returning the request
    identifier and an optional error code, None if the request was
    written successfully. *)

val disconnect_async : Unix.file_descr -> handle -> unit
(** [disconnect_async fd handle] shuts down the NBD connection but
    does not close the file descriptor. *)
