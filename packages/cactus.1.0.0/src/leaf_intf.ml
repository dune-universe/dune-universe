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
  type t

  type key

  type value

  type store

  type address

  val init : store -> address -> t

  val create : store -> t (* like init but allocates a new page *)

  val load : store -> address -> t

  val self_address : t -> address

  val overflow : t -> bool

  val will_overflow : t -> bool

  val underflow : t -> bool

  val split : t -> key * t
  (** [split leaf] allocates a new leaf to split to, splits [leaf], promotes the middle key, and
      returns [promoted, allocated] *)

  val merge : t -> t -> [ `Partial | `Total ]

  val find : t -> key -> value

  val leftmost : t -> key

  val mem : t -> key -> bool

  val add : t -> key -> value -> unit

  val remove : t -> key -> unit

  val iter : t -> (key -> value -> unit) -> unit

  val length : t -> int
  (** [length t] is the number of bindings in [t] *)

  val migrate : string list -> string

  val pp : t Fmt.t
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) (Value : Data.V) ->
  S
    with type value = Value.t
     and type key = Key.t
     and type store = Store.t
     and type address = Store.address

module type Leaf = sig
  module type S = S

  module Make : MAKER
end
