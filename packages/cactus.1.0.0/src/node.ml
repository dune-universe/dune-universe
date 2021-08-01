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

include Node_intf

module Make : MAKER =
functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  ->
  struct
    module NodeFmt = Vertex.NodeMake (Params) (Store) (Key)
    module CommonFields = Field.MakeCommon (Params)
    module Address = CommonFields.Address

    type key = Key.t

    type address = Store.address

    type store = Store.t

    type kind = Field.kind

    let encode_address = Address.to_t

    let decode_address = Address.from_t

    type t = { store : Store.t; node : NodeFmt.t; address : Store.address }

    let pp ppf t = NodeFmt.pp ppf t.node

    let create store kind =
      let address = Store.allocate store in
      { store; node = NodeFmt.create store kind address; address }

    let load store address = { store; node = NodeFmt.load store address; address }

    let self_address t = t.address

    let depth t = NodeFmt.depth t.node

    let overflow t = NodeFmt.length t.node > 2 * Params.fanout

    let will_overflow t = NodeFmt.length t.node >= 2 * Params.fanout

    let underflow t = NodeFmt.length t.node < Params.fanout

    let split t =
      let address = Store.allocate t.store in
      let k, node = NodeFmt.split t.node address in
      (k, { store = t.store; node; address })

    let merge t1 t2 =
      let partial = NodeFmt.length t1.node + NodeFmt.length t2.node >= 2 * Params.fanout in
      NodeFmt.merge t1.node t2.node (if partial then `Partial else `Total);
      if not partial then Store.deallocate t2.store t2.address;
      if partial then `Partial else `Total

    let leftmost t = NodeFmt.leftmost t.node

    let find t key = NodeFmt.find t.node key |> decode_address

    type neighbour = {
      main : Key.t * Store.address;
      neighbour : (Key.t * Store.address) option;
      order : [ `Lower | `Higher ];
    }

    let find_with_neighbour t key =
      let ({ main; neighbour; order } : NodeFmt.with_neighbour) =
        NodeFmt.find_with_neighbour t.node key
      in
      let k1, v1 = main in
      let neighbour =
        match neighbour with None -> None | Some (k2, v2) -> Some (k2, v2 |> decode_address)
      in
      { main = (k1, v1 |> decode_address); neighbour; order }

    let add t key address = address |> encode_address |> NodeFmt.add t.node key

    let replace t k1 k2 = NodeFmt.replace t.node k1 k2

    let remove t key = NodeFmt.remove t.node key

    let migrate kvs depth = NodeFmt.migrate kvs depth

    let reconstruct t kind kvs =
      NodeFmt.reconstruct t.node kind (List.map (fun (k, v) -> (k, encode_address v)) kvs)

    let length t = NodeFmt.length t.node

    let iter t func =
      let f key address = address |> decode_address |> func key in
      NodeFmt.iter t.node f

    let fold_left func acc t =
      let func acc (key, address) = func acc key (address |> Address.from_t) in
      NodeFmt.fold_left func acc t.node
  end
