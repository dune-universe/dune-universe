(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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

module type S = sig
  module Btree : Btree.S

  type t = {
    name : string;
    synopsis : string;
    exec : Btree.t -> unit -> unit;
    dependency : string option;
    kind : [ `RW | `R ];
    speed : [ `Quick | `Slow ];
  }

  type suite = t list

  val name : t -> string

  val suite : suite

  val split : suite -> suite list
  (* splits a suite into its connected dependency subgraphs, each sorted in topological order *)

  val run : t -> string -> Btree.cache -> Results.t

  val minimal_filter : t -> bool
end

module type CONFIG = sig
  val config : Input.config
end

module type MAKER = functor (Config : CONFIG) -> S

module type Benchmark = sig
  module type S = S

  module Make : MAKER
end
