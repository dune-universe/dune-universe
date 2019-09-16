(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
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

(** Lwt async interface to MacOS X userlevel network bridging. *)

(** [mode] controls the level of sharing exposed to the vmnet interface.

    - {!Host_mode} lets the guest network interface communicate with other
    guest network interfaces in the host mode and to the native host.
    - {!Shared_mode} lets the guest network interface reach the Internet
    using a network address translator.

    Note that in MacOS X Yosemite, {!Host_mode} also provides a NAT to the
    guest, but with the subnet and DNS options not set (so it has no way
    to communicate externally but can still retrieve host-local network
    configuration via DHCP). *)
type mode = Vmnet.mode =
 | Host_mode
 | Shared_mode [@@deriving sexp]

(** [error] represents hard failures from the underlying vmnet functions. *)
type error = Vmnet.error =
 | Failure
 | Mem_failure
 | Invalid_argument
 | Setup_incomplete
 | Invalid_access
 | Packet_too_big
 | Buffer_exhausted
 | Too_many_packets
 | Unknown of int [@@deriving sexp]

(** [Error] can be raised by vmnet functions when hard errors are encountered. *)
exception Error of error [@@deriving sexp]

(** [Permission_denied] can be raised if the process needs root privileges
    (or the vmnet capability) *)
exception Permission_denied

(** [t] is the internal state of one vmnet interface, including Lwt-specific
   waiters and threads. *)
type t [@@deriving sexp_of]

(** [mac t] will return the MAC address bound to the guest network interface. *)
val mac : t -> Macaddr.t

(** [mtu t] will return the Maximum Transmission unit bound to the guest
    network interface [t]. *)
val mtu : t -> int

(** [max_packet_size t] will return the maximum allowed packet buffer that can
    be passed to {!write}.  Exceeding this will raise {!Packet_too_big} from
    {!write}. *)
val max_packet_size: t -> int

(** [init ?mode] will initialise a fresh vmnet interface, defaulting to
    {!Shared_mode} for the output. Raises {!Error} if something goes wrong. *)
val init : ?mode:mode -> unit -> t Lwt.t

(** [read t buf] will read a network packet into the [buf] {!Cstruct.t} and
   return a fresh subview that represents the packet with the correct length
   and offset. It blocks until a packet is available. *)
val read : t -> Cstruct.t -> Cstruct.t Lwt.t

(** [write t buf] will transmit a network packet contained in [buf].  This will
   normally not block, but the vmnet interface isnt clear on whether this might
   happen. *)
val write : t -> Cstruct.t -> unit Lwt.t
