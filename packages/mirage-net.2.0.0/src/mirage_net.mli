(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
 * Copyright (c) 2018-2019 Hannes Mehnert <hannes@mehnert.org>
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

(** Network devices

    [Mirage_net] defines the signature for MirageOS network devices.

    {e Release v2.0.0 } *)

module Net : sig
  type error = [ Mirage_device.error | `Invalid_length ]
  (** The type for IO operation errors *)

  val pp_error: error Fmt.t
  (** [pp_error] pretty-print network errors. *)
end

type stats = {
  mutable rx_bytes: int64;
  mutable rx_pkts: int32;
  mutable tx_bytes: int64;
  mutable tx_pkts: int32;
}
(** The type for frame statistics to track the usage of the device. *)

(** {2 Networking} *)

(** A network interface that serves Ethernet frames. *)
module type S = sig

  type error = private [> Net.error]
  (** The type for network interface errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type buffer
  (** The type for memory buffers. *)

  type macaddr
  (** The type for unique MAC identifiers for the network interface. *)

  include Mirage_device.S

  val write: t -> size:int -> (buffer -> int) -> (unit, error) result io
  (** [write net ~size fill] allocates a buffer of length [size], where [size]
     must not exceed the interface maximum packet size ({!mtu} plus Ethernet
     header). The allocated buffer is zeroed and passed to the [fill] function
     which returns the payload length, which may not exceed the length of the
     buffer. When [fill] returns, a sub buffer is put on the wire: the allocated
     buffer from index 0 to the returned length. *)

  val listen: t -> header_size:int -> (buffer -> unit io) -> (unit, error) result io
  (** [listen ~header_size net fn] waits for a [packet] with size at most
     [header_size + mtu] on the network device. When a [packet] is received, an
     asynchronous task is created in which [fn packet] is called. The ownership
     of [packet] is transferred to [fn].  The function can be stopped by calling
     [disconnect] in the device layer. *)

  val mac: t -> macaddr
  (** [mac net] is the MAC address of [net]. *)

  val mtu: t -> int
  (** [mtu net] is the Maximum Transmission Unit of [net]. This excludes the
     Ethernet header. *)

  val get_stats_counters: t -> stats
  (** Obtain the most recent snapshot of the interface statistics. *)

  val reset_stats_counters: t -> unit
  (** Reset the statistics associated with this interface to their
      defaults. *)

end

module Stats : sig
  val create: unit -> stats
  (** [create ()] returns a fresh set of zeroed counters *)

  val rx: stats -> int64 -> unit
  (** [rx t size] records that we received a packet of length [size] *)

  val tx: stats -> int64 -> unit
  (** [tx t size] records that we transmitted a packet of length [size] *)

  val reset: stats -> unit
  (** [reset t] resets all packet counters in [t] to 0 *)
end
