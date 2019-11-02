(*
 * Copyright (c) 2013,2014 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std

type id = [
| `Client of int (* device id *)
| `Server of int * int (* domid * device id *)
] [@@deriving sexp]

type backend_configuration = {
  frontend_id: int;
  backend_id: int;
  backend: string;
  features_available: Features.t;
} [@@deriving sexp]

type frontend_configuration = {
  tx_ring_ref: int32;
  rx_ring_ref: int32;
  event_channel: string;
  feature_requests: Features.t;
} [@@deriving sexp]

module type CONFIGURATION = sig
  val read_frontend_mac: id -> Macaddr.t Lwt.t
  val read_backend_mac: id -> Macaddr.t Lwt.t

  val read_mtu: id -> int Lwt.t

  val read_frontend_configuration: id -> frontend_configuration Lwt.t
  (** Waits for the frontend configuration to become available
      and returns it. *)

  val write_frontend_configuration: id -> frontend_configuration -> unit Lwt.t

  val enumerate: unit -> string list Lwt.t
  (** List the names of available devices. *)

  val connect: id -> unit Lwt.t

  val disconnect_frontend: id -> unit Lwt.t
  (** Set the frontend state to Closed. *)

  val disconnect_backend: id -> unit Lwt.t
  (** Delete the backend directory. *)

  val wait_until_backend_connected: backend_configuration -> unit Lwt.t

  val read_backend: id -> backend_configuration Lwt.t

  val init_backend: id -> Features.t -> backend_configuration Lwt.t
  (** Initialise the configuration for a new backend. *)

  val description: string
  (** Human-readable description suitable for help text or
      a manpage *)

  val wait_for_frontend_closing: id -> unit Lwt.t
  (** [wait_for_frontend_closing id] is a thread that returns when
      [id]'s frontend moves to the closing state. *)

  val wait_for_backend_closing: id -> unit Lwt.t
  (** [wait_for_backend_closing id] is a thread that returns when
      [id]'s backend moves to the closing state. *)
end
