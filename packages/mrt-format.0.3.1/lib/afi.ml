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

type tc =
  | IP4
  | IP6
  | UNKNOWN of int
let tc_to_int = function
  | IP4 -> 1
  | IP6 -> 2
  | UNKNOWN v -> v
and int_to_tc = function
  | 1 -> IP4
  | 2 -> IP6
  | v -> UNKNOWN v

and tc_to_string = function
  | IP4 -> "IPv4"
  | IP6 -> "IPv6"
  | UNKNOWN v -> sprintf "UNKNOWN %d" v

[%%cstruct
  type ip4 = {
    ip: uint32_t;
  }
  [@@big_endian]
]

[%%cstruct
  type ip6 = {
    hi: uint64_t;
    lo: uint64_t;
  }
  [@@big_endian]
]

type ip4 = int32
let ip4_to_string ip =
  sprintf "%ld.%ld.%ld.%ld"
    (ip >>> 24 &&& 0xff_l) (ip >>> 16 &&& 0xff_l)
    (ip >>>  8 &&& 0xff_l) (ip        &&& 0xff_l)

type ip6 = int64 * int64
let ip6_to_string (hi,lo) =
  sprintf "%04Lx:%04Lx:%04Lx:%04Lx:%04Lx:%04Lx:%04Lx:%04Lx"
    (hi >>>> 48 &&&& 0xffff_L) (hi >>>> 32 &&&& 0xffff_L)
    (hi >>>> 16 &&&& 0xffff_L) (hi         &&&& 0xffff_L)
    (lo >>>> 48 &&&& 0xffff_L) (lo >>>> 32 &&&& 0xffff_L)
    (lo >>>> 16 &&&& 0xffff_L) (lo         &&&& 0xffff_L)

type ip = IPv4 of ip4 | IPv6 of ip6
let ip_to_string = function
  | IPv4 ip -> ip4_to_string ip
  | IPv6 ip -> ip6_to_string ip

type prefix = ip * Cstruct.uint8
let prefix_to_string (ip,l) = sprintf "%s/%d" (ip_to_string ip) l
