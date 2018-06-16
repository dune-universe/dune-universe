(*
 * Copyright (c) 2012 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012 Citrix Systems Inc
 * Copyright (C) 2013 Richard Mortier <mort@cantab.net>
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

(** PCAP encoding and encoding

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}}
*)

val major_version: int
(** Major version of the pcap format which we understand *)

val minor_version: int
(** Minor version of the pcap format which we understand *)

type endian =
  | Big     (** Big endian (pcap headers) *)
  | Little  (** Little endian (pcap headers) *)

val string_of_endian : endian -> string

val sizeof_pcap_header: int
(** The size of the initial pcap header in bytes *)

val sizeof_pcap_packet: int
(** The size of the per-packet pcap headers in bytes *)

val magic_number: int32
(** The magic number which identifies a pcap file (and endian-ness) *)

module Network : sig
  (** Type of outermost network protocol within the captured frames *)

  type t =
    | Ethernet
    | Ieee80211

  val to_int32: t -> int32

  val of_int32: int32 -> t option

end

module LE : sig

  val endian : endian

  [%%cstruct
      type pcap_header = {
        magic_number: uint32_t;  (* magic number *)
        version_major: uint16_t; (* major version number *)
        version_minor: uint16_t; (* minor version number *)
        thiszone: uint32_t;      (* GMT to local correction *)
        sigfigs: uint32_t;       (* accuracy of timestamps *)
        snaplen: uint32_t;       (* max length of captured packets, octets *)
        network: uint32_t        (* data link type *)
      } [@@little_endian]
  ]

  [%%cstruct
      type pcap_packet = {
        ts_sec: uint32_t;       (* timestamp seconds *)
        ts_usec: uint32_t;      (* timestamp microseconds *)
        incl_len: uint32_t;     (* number of octets of packet saved in file *)
        orig_len: uint32_t      (* actual length of packet *)
      } [@@little_endian]
  ]

end

module BE : sig

  val endian : endian

  [%%cstruct
      type pcap_header = {
        magic_number: uint32_t;  (* magic number *)
        version_major: uint16_t; (* major version number *)
        version_minor: uint16_t; (* minor version number *)
        thiszone: uint32_t;      (* GMT to local correction *)
        sigfigs: uint32_t;       (* accuracy of timestamps *)
        snaplen: uint32_t;       (* max length of captured packets, octets *)
        network: uint32_t        (* data link type *)
      } [@@big_endian]
  ]

  [%%cstruct
      type pcap_packet = {
        ts_sec: uint32_t;       (* timestamp seconds *)
        ts_usec: uint32_t;      (* timestamp microseconds *)
        incl_len: uint32_t;     (* number of octets of packet saved in file *)
        orig_len: uint32_t      (* actual length of packet *)
      } [@@big_endian]
  ]

end

module type HDR = sig
  (** Functions to read/write pcap header fields of a particular
      endian-ness *)

  val endian: endian
  (** The detected endian-ness of the headers *)

  val get_pcap_header_magic_number: Cstruct.t -> int32
  val get_pcap_header_version_major: Cstruct.t -> int
  val get_pcap_header_version_minor: Cstruct.t -> int
  val get_pcap_header_thiszone: Cstruct.t -> int32
  val get_pcap_header_sigfigs: Cstruct.t -> int32
  val get_pcap_header_snaplen: Cstruct.t -> int32
  val get_pcap_header_network: Cstruct.t -> int32

  val set_pcap_header_magic_number: Cstruct.t -> int32 -> unit
  val set_pcap_header_version_major: Cstruct.t -> int -> unit
  val set_pcap_header_version_minor: Cstruct.t -> int -> unit
  val set_pcap_header_thiszone: Cstruct.t -> int32 -> unit
  val set_pcap_header_sigfigs: Cstruct.t -> int32 -> unit
  val set_pcap_header_snaplen: Cstruct.t -> int32 -> unit
  val set_pcap_header_network: Cstruct.t -> int32 -> unit

  val get_pcap_packet_ts_sec: Cstruct.t -> int32
  val get_pcap_packet_ts_usec: Cstruct.t -> int32
  val get_pcap_packet_incl_len: Cstruct.t -> int32
  val get_pcap_packet_orig_len: Cstruct.t -> int32

  val set_pcap_packet_ts_sec: Cstruct.t -> int32 -> unit
  val set_pcap_packet_ts_usec: Cstruct.t -> int32 -> unit
  val set_pcap_packet_incl_len: Cstruct.t -> int32 -> unit
  val set_pcap_packet_orig_len: Cstruct.t -> int32 -> unit

end

val detect: Cstruct.t -> (module HDR) option
(** [detect buf] returns a module capable of reading the pcap header fields, or
    None if the buffer doesn't contain pcap data. *)

val packets: (module HDR) -> Cstruct.t -> (Cstruct.t * Cstruct.t) Cstruct.iter
(** [packets hdr buf] returns a Cstruct.iter (sequence) containing
    (pcap header, pcap body) pairs. *)
