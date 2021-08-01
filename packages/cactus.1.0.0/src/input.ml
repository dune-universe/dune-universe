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

module type Size = sig
  val fanout : int

  val version : int

  val page_sz : int

  val cache_sz : int

  val debug : bool

  module Debug : sig
    val random_failure : bool
  end
end

module type Key = sig
  type t [@@deriving repr]
  (** The type for keys. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number of bytes. *)

  val decode : string -> t
  (** [decode s] is the decoded form of the encoded value of string [s]. Must satisfy
      [decode (encode t) = t]. *)
end

module type Value = sig
  type t [@@deriving repr]
  (** The type for values. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number of bytes. *)

  val decode : string -> t
  (** [decode s off] is the decoded form of the encoded value at the offset [off] of string [s].
      Must satisfy [decode (encode t) 0 = t]. *)
end

module Default = struct
  module Size : Size = struct
    let fanout = 25

    let version = 0

    let page_sz = 2700 (* page size, in bytes *)

    let cache_sz = 1_000 (* allowed memory, in MB *)

    let debug = false

    module Debug = struct
      let random_failure = false
    end
  end

  module Key : Key = struct
    type t = string [@@deriving repr]

    let encoded_size = 30

    let encode s =
      assert (String.length s = encoded_size);
      s

    let decode s =
      assert (String.length s = encoded_size);
      s
  end

  module Value : Value = struct
    type t = int [@@deriving repr]

    let encoded_size = 15

    let encode i =
      i |> Utils.to_b256 |> fun s -> String.make (encoded_size - String.length s) '\000'

    let decode s =
      assert (String.length s = encoded_size);
      s |> Utils.from_b256
  end
end
