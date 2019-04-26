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
open Operators

[%%cenum
  type tc =
    | PEER_INDEX_TABLE [@id 1]
    | RIB_IPV4_UNICAST
    | RIB_IPV4_MULTICAST
    | RIB_IPV6_UNICAST
    | RIB_IPV6_MULTICAST
    (* RIB_GENERIC        = 6 *)
  [@@uint8_t]
]

[%%cstruct
  type index_table = {
    bgpid: uint32_t ;
  }
  [@@big_endian]
]

let get_string16 buf =
  let name_len = Cstruct.BE.get_uint16 buf 0 in
  let name = Cstruct.(to_string (sub buf 2 name_len)) in
  let rest = Cstruct.shift buf (2+name_len) in
  name, rest

[%%cstruct
  type peer = {
    typ: uint8_t;
    bgpid: uint32_t;
  }
  [@@big_endian]
]

type peer = {
  id: int32;
  ip: Afi.ip;
  asn: Bgp.asn;
}

let peer_to_string p =
  sprintf "id:0x%08lx, ip:%s, asn:%s"
    p.id (Afi.ip_to_string p.ip) (Bgp.asn_to_string p.asn)

let rec peers_to_string ps = match ps () with
  | None -> ""
  | Some p -> (peer_to_string p) ^ "; " ^ (peers_to_string ps)

[%%cstruct
  type rib_h = {
    seqno: uint32_t
  }
  [@@big_endian]
]

[%%cstruct
  type rib = {
    peer: uint16_t;
    otime: uint32_t;
    alen: uint16_t;
  }
  [@@big_endian]
]

type rib = {
  peer_index: int; (* 16 bit, index into peer_table *)
  otime: int32;
  attrs: Bgp.path_attrs;
}

let rib_to_string r =
  sprintf "peer:%d, otime:%lu, attrs:[%s]"
    r.peer_index r.otime (Bgp.path_attrs_to_string r.attrs)

let rec ribs_to_string rs = match rs () with
  | None -> ""
  | Some r -> (rib_to_string r) ^ "; " ^ (ribs_to_string rs)

type header = unit

type payload =
  | Index_table of int32 * string * peer Cstruct.iter
  | Ip4_unicast of int32 * Afi.prefix * rib Cstruct.iter
  | Ip4_multicast of int32 * Afi.prefix * rib Cstruct.iter
  | Ip6_unicast of int32 * Afi.prefix * rib Cstruct.iter
  | Ip6_multicast of int32 * Afi.prefix * rib Cstruct.iter

type t = header * payload

let to_string (_,p) =
  let payload_to_string = function
    | Index_table (id, n, peers) ->
      sprintf "INDEX_TABLE(bgpid:0x%08lx, name:\"%s\", peers:[%s])"
        id n (peers_to_string peers)

    | Ip4_unicast (seqno, prefix, ribs) ->
      sprintf "IPV4_UNICAST(seqno:%ld, prefix:%s, ribs:[%s])"
        seqno (Afi.prefix_to_string prefix) (ribs_to_string ribs)

    | Ip4_multicast (seqno, prefix, ribs) ->
      sprintf "IPV4_MULTICAST(seqno:%ld, prefix:%s, ribs:[%s])"
        seqno (Afi.prefix_to_string prefix) (ribs_to_string ribs)

    | Ip6_unicast (seqno, prefix, ribs) ->
      sprintf "IPV6_UNICAST(seqno:%ld, prefix:%s, ribs:[%s])"
        seqno (Afi.prefix_to_string prefix) (ribs_to_string ribs)

    | Ip6_multicast (seqno, prefix, ribs) ->
      sprintf "IPV6_MULTICAST(seqno:%ld, prefix:%s, ribs:[%s])"
        seqno (Afi.prefix_to_string prefix) (ribs_to_string ribs)
  in
  sprintf "TABLE2()|%s" (payload_to_string p)

let parse subtype buf =
  let parse_ribs hlen buf =
    let buf = Cstruct.shift buf hlen in
    Cstruct.iter
      (fun buf -> Some (sizeof_rib + (get_rib_alen buf)))
      (fun buf ->
         let peer_index = get_rib_peer buf in
         let otime = get_rib_otime buf in
         let attrs = Bgp.parse_path_attrs ~caller:Bgp.Table2
             (Cstruct.shift buf sizeof_rib)
         in
         { peer_index; otime; attrs }
      )
      buf
  in
  let lenf buf = Some (Cstruct.len buf) in
  let pf buf = (), (match int_to_tc subtype with
      | None -> failwith "pf: bad TABLE2 payload"

      | Some PEER_INDEX_TABLE ->
        let itid = get_index_table_bgpid buf in
        let buf = Cstruct.shift buf sizeof_index_table in
        let viewname, buf = get_string16 buf in
        let buf = Cstruct.shift buf 2 in
        let parse_peer_entries = Cstruct.(
            iter
              (fun buf ->
                 let pt = get_peer_typ buf in
                 let plen = (if is_bit 0 (* 7 *) pt then 16 else 4)
                            + (if is_bit 1 (* 6 *) pt then 4 else 2)
                 in
                 Some (sizeof_peer + plen)
              )
              (fun buf ->
                 let hlen = sizeof_peer in
                 let h,p = Cstruct.split buf hlen in
                 let pt = get_peer_typ h in
                 let id = get_peer_bgpid h in
                 let ip, sz = Afi.(match is_bit 0 (* 7 *) pt with
                     | false -> IPv4 (get_ip4_ip p), sizeof_ip4
                     | true -> IPv6 (get_ip6_hi p, get_ip6_lo p), sizeof_ip6
                   ) in
                 let p = Cstruct.shift p sz in
                 let asn = Bgp.(match is_bit 1 (* 6 *) pt with
                     | false -> Asn (Cstruct.BE.get_uint16 p 0)
                     | true -> Asn4 (Cstruct.BE.get_uint32 p 0)
                   ) in
                 { id; ip; asn }
              )
              buf
          )
        in
        Index_table (itid, viewname, parse_peer_entries)

      | Some RIB_IPV4_UNICAST ->
        let seqno = get_rib_h_seqno buf in
        let (ip,pl) = Bgp.get_nlri4 buf sizeof_rib_h in
        let hlen = sizeof_rib_h + 1 + (Bgp.pfxlen_to_bytes pl) + 2 in
        Ip4_unicast (seqno, (ip,pl), parse_ribs hlen buf)

      | Some RIB_IPV4_MULTICAST ->
        let seqno = get_rib_h_seqno buf in
        let (ip,pl) = Bgp.get_nlri4 buf sizeof_rib_h in
        let hlen = sizeof_rib_h + 1 + (Bgp.pfxlen_to_bytes pl) + 2 in
        Ip4_multicast (seqno, (ip,pl), parse_ribs hlen buf)

      | Some RIB_IPV6_UNICAST ->
        let seqno = get_rib_h_seqno buf in
        let (ip,pl) = Bgp.get_nlri6 buf sizeof_rib_h in
        let hlen = sizeof_rib_h + 1 + (Bgp.pfxlen_to_bytes pl) + 2 in
        Ip6_unicast (seqno, (ip,pl), parse_ribs hlen buf)

      | Some RIB_IPV6_MULTICAST ->
        let seqno = get_rib_h_seqno buf in
        let (ip,pl) = Bgp.get_nlri6 buf sizeof_rib_h in
        let hlen = sizeof_rib_h + 1 + (Bgp.pfxlen_to_bytes pl) + 2 in
        Ip6_multicast (seqno, (ip,pl), parse_ribs hlen buf)
    )
  in
  Cstruct.iter lenf pf buf
