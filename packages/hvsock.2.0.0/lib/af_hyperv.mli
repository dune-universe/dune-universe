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

(** Low-level interface to the AF_HYPERV socket family available on Windows kernels. *)


type vmid =
  | Wildcard      (** Any partition *)
  | Children      (** Any child partition *)
  | Loopback      (** The same partition *)
  | Parent        (** The parent partition *)
  | Id of Uuidm.t (** A specific VM id *)
(** A vmid identifies a VM, also known as a partition *)

val string_of_vmid: vmid -> string

type serviceid = string
(** A serviceid identifies a service (like a port number) *)

type sockaddr = {
  vmid: vmid;           (** identifies a partition *)
  serviceid: serviceid; (** identifies a service *)
}
(** An AF_HVSOCK socket address *)

include Af_common.S
  with type sockaddr := sockaddr
   and type t = Unix.file_descr

val vmid_of_name: string -> Uuidm.t
(** Look up a vmid given a VM's human-readable name.
    This function requires Administrator privileges. *)

val register_serviceid: string -> unit
(** Register the serviceid in the registry.
    This function requires Administrator privileges. *)
