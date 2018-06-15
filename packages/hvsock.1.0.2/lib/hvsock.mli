(*
 * Copyright (C) 2016 Docker Inc
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
 *
 *)

type vmid =
  | Wildcard      (** Any partition *)
  | Children      (** Any child partition *)
  | Loopback      (** The same partition *)
  | Parent        (** The parent partition *)
  | Id of string  (** A specific VM id *)

val string_of_vmid: vmid -> string

type sockaddr = {
  vmid: vmid;         (** identifies a partition *)
  serviceid: string;  (** identifies a service *)
}
(** An AF_HVSOCK socket address *)

val create: unit -> Unix.file_descr
(** [create ()] creates an unbound AF_HVSOCK socket *)

val bind: Unix.file_descr -> sockaddr -> unit
(** [bind socket sockaddr] binds [socket] to [sockaddr] *)

val accept: Unix.file_descr -> Unix.file_descr * sockaddr
(** [accept fd] accepts a single connection *)

val connect: ?timeout_ms:int -> Unix.file_descr -> sockaddr -> unit
(** [connect ?timeout_ms fd sockaddr] connects to a remote partition.
    Since the raw socket call can block forever if the server is not
    running when the call is executed (even if the server starts up afterwards)
    there is a default timeout of 300ms. On timeout this will raise
    [Unix_error(Unix.ETIMEDOUT)] *)
