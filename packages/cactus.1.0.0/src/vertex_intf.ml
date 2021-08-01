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
  (** The type for vertices. For leafs its a map (key * value). For nodes it maps a [key] interval
      to an [address]. *)

  type key

  type value

  type address

  type store

  val create : store -> Field.kind -> address -> t
  (** [create s k p] creates a new empty vertex (of kind either Leaf or Node), stored at address [p]
      in [s]. *)

  val load : store -> address -> t
  (** [load s p] loads the table stored at address [p] in [s]. *)

  val reconstruct : t -> Field.kind -> (key * value) list -> unit
  (** [reconstruct t kind kvs] overwrite [t] with the list of bindings [kvs] which is assumed to be
      sorted. *)

  val migrate : string list -> Field.kind -> string
  (** [migrate kvs kind] is the representation of the key-value association list [kvs] in a vertex
      of type [kind] *)

  val leftmost : t -> key
  (** [leftmost t] is the smallest key bound in [t] *)

  val split : t -> address -> key * t
  (** [split t p] moves half of the bindings from [t] (with keys larger than a pivot, which is the
      middle key bounded in [t]) to a new table [t_mv] stored at address [p]. Return [pivot, t_mv]. *)

  val replace : t -> key -> key -> unit
  (** [replace t k1 k2] replaces key [k1] in [t] with [k2] *)

  val add : t -> key -> value -> unit
  (** [add t k v] adds a binding from [k] to [v] in [t]. Contrary to [Map.add], previous bindings
      from [k] are not hidden, but deleted. *)

  val find : t -> key -> value
  (** [find t k] returns the current binding of [k] in [t], or raises [Not_found] if no such binding
      exists. *)

  type with_neighbour = {
    main : key * value;
    neighbour : (key * value) option;
    order : [ `Lower | `Higher ];
  }

  val find_with_neighbour : t -> key -> with_neighbour
  (** [find_with_neighbour t k] as [find t k] but also returns a neighbour of [k] in [t] (the
      right-most one when [k] has neighbours in both directions). *)

  val mem : t -> key -> bool
  (** [mem t k] checks if [k] is bound in [t] in case the vertex is a leaf and raises
      [Invalid_argument] if its a node. *)

  val iter : t -> (key -> value -> unit) -> unit
  (** [iter t func] applies [func key value] on every bindings [(key, value)] stored in [t] *)

  val fold_left : ('a -> key * value -> 'a) -> 'a -> t -> 'a

  val merge : t -> t -> [ `Partial | `Total ] -> unit
  (** [merge t1 t2 mode] merges bindings in [t1] and [t2]. A partial merge merely redistribute the
      keys evenly among the nodes, a total merge moves all keys from [t2] to [t1]. It is assumed,
      and relied upon, that all keys from [t2] are greater than every key from [t1]. *)

  val remove : t -> key -> unit
  (** [remove t k] removes the binding of [k] in [t], or raises [Not_found] if no such binding
      exists. *)

  val length : t -> int
  (** [length t] is the number of keys bound in [t]. It takes constant time. *)

  val depth : t -> int
  (** [depth t] is the depth of the vertex [t]. *)

  val pp : t Fmt.t
  (** [pp ppf t] outputs a human-readable representation of [t] to the formatter [ppf] *)
end

module type VALUE = sig
  type t

  val set : marker:(unit -> unit) -> bytes -> off:int -> t -> unit

  val get : bytes -> off:int -> t

  val size : int

  val pp : t Fmt.t

  val kind : [ `Leaf | `Node ]
end

module type LEAFMAKER = functor
  (Params : Params.S)
  (Store : Store.S)
  (Key : Data.K)
  (Value : Data.V)
  ->
  S
    with type key := Key.t
     and type value := Value.t
     and type store = Store.t
     and type address = Store.address

module type NODEMAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) ->
  S
    with type key := Key.t
     and type value := Field.MakeCommon(Params).Address.t
     and type store = Store.t
     and type address = Store.address

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Key : Data.K) (Value : VALUE) ->
  S
    with type key := Key.t
     and type value := Value.t
     and type store = Store.t
     and type address = Store.address

module type Vertex = sig
  module LeafMake : LEAFMAKER

  module NodeMake : NODEMAKER
end
