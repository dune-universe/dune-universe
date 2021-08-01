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

include Btree_index_intf

module Make (Key : Key) (Value : Value) = struct
  module Key : Input.Key with type t = Key.t = struct
    include Key

    let decode s = decode s 0
  end

  module Value : Input.Value with type t = Value.t = struct
    include Value

    let decode s = decode s 0
  end

  include Ext.Make_ext (Key) (Value) (Input.Default.Size)

  exception NotInBtree of string

  let try_merge _t = raise (NotInBtree "try_merge")

  let merge _t = () (* raise (NotInBtree "merge") *)

  let is_merging _t = raise (NotInBtree "is_merging")

  let sync _t = raise (NotInBtree "sync")

  let close ?immediately t =
    ignore immediately;
    close t

  let filter _t _cond = raise (NotInBtree "filter")

  let replace ?overcommit t key value =
    ignore overcommit;
    add t key value

  let v ?flush_callback ?cache ?fresh ?readonly ?throttle ~log_size root =
    ignore flush_callback;
    ignore fresh;
    ignore readonly;
    ignore throttle;
    ignore log_size;
    match cache with None -> create root | Some cache -> create ~cache root

  let flush ?no_callback ?with_fsync t =
    ignore no_callback;
    ignore with_fsync;
    ignore t
end
