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

type tc =
  | UNICAST
  | MULTICAST
  | UNKNOWN of int

let tc_to_int = function
  | UNICAST -> 1
  | MULTICAST -> 2
  | UNKNOWN v -> v
and int_to_tc = function
  | 1 -> UNICAST
  | 2 -> MULTICAST
  | v -> UNKNOWN v

and tc_to_string = function
  | UNICAST -> "UNICAST"
  | MULTICAST -> "MULTICAST"
  | UNKNOWN v -> sprintf "UNKNOWN %d" v
