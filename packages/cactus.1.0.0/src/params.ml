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
  val fanout : int
  (** The fanout characterizes the number of bindings in a vertex, which is bounded between fanout
      and 2 * fanout for all vertexes except for the root of the btree. *)

  val version : int
  (** The version of the btree. *)

  val tree_height_sz : int
  (** The number of bytes for representing the tree height. *)

  val page_sz : int
  (** The page size (in B). It should be (approx) the max between (max_key * (key_sz +
      page_address_sz)) and (max_key * (key_sz + value_sz)). *)

  val cache_sz : int
  (** The cache size (in MB). *)

  val key_sz : int
  (** The number of bytes for representing keys. *)

  val value_sz : int
  (** The number of bytes for representing values. *)

  val max_key : int
  (** The maximum number of keys per page (usually 2 * fanout + 1). *)

  val max_key_sz : int
  (** The number of bytes for representing the max_key. *)

  val version_sz : int
  (** The number of bytes for representing versions. *)

  val btree_magic : string
  (** Magic string written on disk to mark the header of a btree file. *)

  val page_magic : string
  (** Magic string written on disk to mark the header of a page. *)

  val magic_sz : int
  (** Magic size (in B). *)

  val key_repr_sz : int
  (** The number of bytes for representing the key_sz. Useful for variable length keys or for the
      (not yet implemented) suffix truncation of keys. *)

  val page_address_sz : int
  (** The number of bytes for representing page addresses. *)

  val pointer_sz : int
  (** The number of bytes for representing pointers. *)

  val debug : bool
  (** Specifies if the run is in debug mode which checks sortedness after key insertion and
      deletion. *)

  module Debug : sig
    val random_failure : bool
  end
end

module Constant = struct
  let version_sz = 1

  let btree_magic = "TREE"

  let page_magic = "PAGE"

  let tree_height_sz = 2

  let page_address_sz = 4

  let pointer_sz = 4

  let flag = 1
end

module Make : functor (I : Input.Size) (Key : Input.Key) (Value : Input.Value) -> S =
functor
  (I : Input.Size)
  (Key : Input.Key)
  (Value : Input.Value)
  ->
  struct
    include I
    include Constant

    let value_sz = Value.encoded_size

    let key_sz = Key.encoded_size

    let max_key = (2 * fanout) + 1

    let max_key_sz = max_key |> Utils.b256size

    let magic_sz = String.(max (length btree_magic) (length page_magic))

    let key_repr_sz = key_sz |> Utils.b256size

    let () =
      if 1 lsl (4 * pointer_sz) < page_sz then
        failwith "Pages are too large to be fully addressable"
  end
