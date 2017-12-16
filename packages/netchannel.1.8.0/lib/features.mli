(*
 * Copyright (c) 2013-2015 Citrix Systems Inc
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

type t = {
  rx_copy: bool;
  rx_flip: bool;
  rx_notify: bool;  (* Client sends notifications for rx requests. *)
  sg: bool;         (* Scatter/gather IO *)
  gso_tcpv4: bool;  (* Can handle large TCP packets *)
  smart_poll: bool;
  (* Unsupported:
     - split-event-channels (TX and RX notifications use separate channels)
     - multi-queue-{max,num}-queues
     - no-csum-offload      (IPv4 TCP/UDP checksum offload)
     - ipv6-csum-offload    (IPv6 TCP/UDP checksum offload)
     - gso_tcpv6
     - multicast-control    (filter ethernet multicast packets in the backend)
     - dynamic-multicast-control (multicast-control can be set at any time)
     - ctrl-ring            (used for some extra features, e.g. hashes and static grants)
   *)
} [@@deriving sexp]
(** All the features of which news has reached harvard; there may be others
    but they haven't been discovered *)

val supported: t
(** Set of features supported by this driver version *)
