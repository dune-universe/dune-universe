(*
 * Copyright (c) 2012-2017 Richard Mortier <mort@cantab.net>
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

open Printf

[%%cstruct
  type h4 = {
    viewno: uint16_t;
    seqno: uint16_t;
    prefix: uint32_t;
    pfxlen: uint8_t;
    status: uint8_t;
    otime: uint32_t;
    peer_ip: uint32_t;
    peer_as: uint16_t;
    attrlen: uint16_t;
  }
  [@@big_endian]
]

[%%cstruct
  type h6 = {
    viewno: uint16_t;
    seqno: uint16_t;
    prefix_hi: uint64_t;
    prefix_lo: uint64_t;
    pfxlen: uint8_t;
    status: uint8_t;
    otime: uint32_t;
    peer_ip_hi: uint64_t;
    peer_ip_lo: uint64_t;
    peer_as: uint16_t;
    attrlen: uint16_t;
  }
  [@@big_endian]
]

type header = {
  viewno: int;
  seqno: int;
  prefix: Afi.ip;
  pfxlen: int;
  status: int;
  otime: int32;
  peer_ip: Afi.ip;
  peer_as: Bgp.asn;
}

let header_to_string h =
  sprintf "viewno:%d, seqno:%d, status:%d, otime:%ld, peer_ip:%s, peer_as:%s, prefix:%s/%d"
    h.viewno h.seqno h.status h.otime
    (Afi.ip_to_string h.peer_ip) (Bgp.asn_to_string h.peer_as)
    (Afi.ip_to_string h.prefix) h.pfxlen

type payload = Not_implemented

let payload_to_string = function
  | Not_implemented -> "Not_implemented"

type t = header * payload

let to_string (h,p) =
  sprintf "TABLE(%s)|%s" (header_to_string h) (payload_to_string p)

let parse subtype buf =
  let lenf buf = Some (Cstruct.len buf) in
  let pf buf =
    let hlen = Afi.(match int_to_tc subtype with
        | IP4 -> sizeof_h4
        | IP6 -> sizeof_h6
      )
    in
    let h,_ = Cstruct.split buf hlen in
    let header = Afi.(match int_to_tc subtype with
        | IP4 -> { viewno=get_h4_viewno h;
                   seqno=get_h4_seqno h;
                   prefix=IPv4 (get_h4_prefix h);
                   pfxlen=get_h4_pfxlen h;
                   status=get_h4_status h;
                   otime=get_h4_otime h;
                   peer_ip=IPv4 (get_h4_peer_ip h);
                   peer_as=Bgp.Asn (get_h4_peer_as h);
                 }
        | IP6 -> { viewno=get_h6_viewno h;
                   seqno=get_h6_seqno h;
                   prefix=IPv6 ((get_h6_prefix_hi h), (get_h6_prefix_lo h));
                   pfxlen=get_h6_pfxlen h;
                   status=get_h6_status h;
                   otime=get_h6_otime h;
                   peer_ip=IPv6 ((get_h6_peer_ip_hi h), (get_h6_peer_ip_lo h));
                   peer_as=Bgp.Asn (get_h6_peer_as h);
                 }
      )
    in
    (header, Not_implemented)
  in
  Cstruct.iter lenf pf buf
