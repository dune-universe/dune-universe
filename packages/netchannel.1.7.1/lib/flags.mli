(*
 * Copyright (c) 2010-2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014-2015 Citrix Inc
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

type t [@@deriving sexp]
(** A set of flags. *)

val empty : t

val checksum_blank : t
(** Indicates that this is a TCP or UDP packet with an incomplete checksum.
    The separate IPv4 header checksum must always be calculated by the frontend.
    If you set this flag for other types of packet (e.g. ARP requests), then
    Linux will silently drop them.

    "You need to set the checksum to the psuedo-header checksum
     rather than 0 since that is what csum_blank means (such a well named
     flag!)"

    "If NETTXF_csum_blank is set (implying NETTXF_data_validated must also be set) 
     then the UDP checksum must be set to cover the UDP pseudo-header since the 
     SKB will be marked CSUM_PARTIAL and thus any h/w driver it is presented to 
     will expect the pseudo-header checksum to be valid. If the SKB is presented 
     to another guest then CSUM_PARTIAL will translated into 
     NETRXF_csum_blank|NETRXF_data_validated and the frontend is at liberty to 
     present it up the stack as being checksum-valid without anything in the 
     datapath having had to walk over the payload to actually calculate the value 
     of that checksum."

    Information collected from this thread:
    http://lists.xenproject.org/archives/html/xen-devel/2011-03/msg01901.html
  *)

val data_validated : t

val more_data : t
(** This request does not contain the entire frame. The following request
    provides more data.
    Linux allows a frame to have up to XEN_NETIF_NR_SLOTS_MIN=18 extra fragments.
    Note: Linux assumes that all fragments are added to the ring at once. *)

val extra_info : t

val of_int: int -> t
val to_int: t -> int

val (++): t -> t -> t
(** [a ++ b] is the union of the sets. *)

val (--): t -> t -> t
(** [a -- b] is the set of flags in [a] but not in [b]. *)

val mem: t -> t -> bool
(** [mem x flags] is [true] iff [x] is a subset of [flags]. *)
