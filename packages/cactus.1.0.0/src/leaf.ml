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

include Leaf_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : Data.V)
  ->
  struct
    module LeafFmt = Vertex.LeafMake (Params) (Store) (Key) (Value)

    type key = Key.t

    type value = Value.t

    type store = Store.t

    type address = Store.address

    type t = { store : Store.t; leaf : LeafFmt.t; address : Store.address }

    let pp ppf t = LeafFmt.pp ppf t.leaf

    let init store address =
      { store; leaf = LeafFmt.create store (Leaf : Field.kind) address; address }

    let create store =
      let address = Store.allocate store in
      init store address

    let load store address = { store; leaf = LeafFmt.load store address; address }

    let self_address t = t.address

    let overflow t = LeafFmt.length t.leaf > 2 * Params.fanout

    let will_overflow t = LeafFmt.length t.leaf >= 2 * Params.fanout

    let underflow t = LeafFmt.length t.leaf < Params.fanout

    let split t =
      let address = Store.allocate t.store in
      let k, leaf = LeafFmt.split t.leaf address in
      (k, { store = t.store; leaf; address })

    let merge t1 t2 =
      let partial = LeafFmt.length t1.leaf + LeafFmt.length t2.leaf >= 2 * Params.fanout in
      LeafFmt.merge t1.leaf t2.leaf (if partial then `Partial else `Total);
      if not partial then Store.deallocate t2.store t2.address;
      if partial then `Partial else `Total

    let leftmost t = LeafFmt.leftmost t.leaf

    let find t key = LeafFmt.find t.leaf key

    let mem t key = LeafFmt.mem t.leaf key

    let add t key value = LeafFmt.add t.leaf key value

    let remove t key = LeafFmt.remove t.leaf key

    let iter t func =
      let f key value = func key value in
      LeafFmt.iter t.leaf f

    let length t = LeafFmt.length t.leaf

    let migrate kvs = LeafFmt.migrate kvs (Leaf : Field.kind)
  end
