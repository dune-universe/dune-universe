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

module Private = struct
  module Utils = Utils
  module Stats = Stats
  module Index_stats = Index_stats
  module Tag = Log.Tag
  module Default = Input.Default
  module Input = Input
  module Data = Data
  module Syscalls = Syscalls
end

module type S = Ext.S

module type MAKER = functor (InKey : Input.Key) (InValue : Input.Value) (Size : Input.Size) ->
  S with type key = InKey.t and type value = InValue.t

module Input = Input

module Index = struct
  module type S = Btree_index.S

  module Make = Btree_index.Make
  module Stats = Btree_index.Stats
end

module type Btree = sig
  module Private = Private

  module type S = S

  module Input = Input

  module Make : MAKER

  module Index = Index
end
