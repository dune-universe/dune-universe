(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module type COLLECTION_BARE = sig

  (** A mutable structure that holds at most a fixed number of values of a same
      type. Values are never removed, once the limit is reached, adding a value
      replaces the oldest-promoted one in the buffer.

      The function [promote] (see below) allows to pull a node to the front of
      the buffer. *)

  (** The type of bounded-size buffers. *)
  type 'a t

  (** [node]s are boxes that contain data. Boxes are never meant to be returned
      to the end user (they can be unsafe), they are meant to build some
      abstraction on top of the buffer.

      In order to make the module safe and remove all notion of box, use the
      functor [Misc.Unbox]. *)
  type 'a node

  (** [data n] is the value contained in the node [n]. *)
  val data : 'a node -> 'a

  (** [create n] allocates a buffer that can hold up to [n] elements.
      @raise [Invalid_argument] if [n] is 0 or less. *)
  val create : int -> 'a t

  (** [capacity b] is the number of elements that [b] can hold. *)
  val capacity : 'a t -> int

  (** [add b v] adds the value [v] to the buffer [b]. If the buffer [b] already
      has [capacity b] values, then a value is dropped. The replacement policy
      of the cache determines which value is dropped, and the overflow policy
      determines if it is dropped immediately or at some point in the future.

      [adds b v] returns the node containing the value [v]. This node can be
      used to [promote] or [remove] the value [v] from the buffer [b]. *)
  val add : 'a t -> 'a -> 'a node

  (** [add_and_return_erased b v] has the same effect as [add b v] but it
      returns the dropped value when applicable (and [None] otherwise). *)
  val add_and_return_erased : 'a t -> 'a -> ('a node * 'a option)

  (** [add_list b vs] adds each element of the list [vs] in the order they
      appear in the list. It returns a list of nodes, for each of the inserted
      elements.

      If [length vs > capacity b], then each value from [vs] is added, but the
      ones at the front of the list are popped. In this case, [add_list b vs]
      returns a list of [capacity b] nodes only. *)
  val add_list : 'a t -> 'a list -> 'a node list

  (** [clear b] removes all values from the buffer [b]. *)
  val clear : 'a t -> unit

  (** [fold b ~init ~f] folds over the value of the buffer [b], oldest to newest.
      *)
  val fold : 'a t -> init:'b -> f:('b -> 'a node -> 'b) -> 'b

  (** [elements b] is a list of nodes from [b]. They appear oldest first, newest
      last. *)
  val elements : 'a t -> 'a node list

  val elements_data : 'a t -> 'a list

  (** [remove b n] removes the node [n] from the buffer [b].

      The behavior of this function is undefined if [n] is not part of [b],
      i.e., if [List.exists ((==) n) (elements b)] is [false].

      It is the responsibility of the user of this library (presumably, another
      library wrapping the primitives of this one) to ensure this is never the
      case. *)
  val remove : 'a t -> 'a node -> unit

  (** [promote b n] places the node [n] to the front of the buffer [b], making
      the node [n] the newest of the nodes of the buffer [b].

      [promote b n] is similar to [remove b n; ignore (add b @@ data n)] except
      that: it is more efficient, and it keeps the value [data n] in the same
      node it was originally inserted in.

      The behavior of this function is undefined if [n] is not part of [b],
      i.e., if [List.exists ((==) n) (elements b)] is [false]. *)
  val promote : 'a t -> 'a node -> unit

end

module type COLLECTION = sig

  include COLLECTION_BARE

  val promote_read : 'a t -> 'a node -> unit

  val promote_write : 'a t -> 'a node -> unit

end

module type UNBOXED_COLLECTION = sig

  (** A mutable structure that holds at most a fixed number of values of a same
      type. Values are not removed by hand, instead, once the limit is reached,
      adding a value replaces the oldest one in the buffer. *)

  (** The type of bounded-size buffers. *)
  type 'a t

  (** [create n] allocates a ring buffer that can hold up to [n] values.
      @raise [Invalid_argument] if [n] is 0 or less. *)
  val create : int -> 'a t

  (** [capacity b] is the number of elements that [b] can hold. *)
  val capacity : 'a t -> int

  (** [add b v] adds the value [v] to the buffer [b]. If the buffer [b] already
      has [capacity b] values, the oldest of its values is dropped. *)
  val add : 'a t -> 'a -> unit

  (** [add_and_return_erased b v] has the same effect as [add b v] but it
      returns the dropped value when applicable. *)
  val add_and_return_erased : 'a t -> 'a -> 'a option

  (** [add_list b vs] adds each element of the list [vs] in the order they
      appear in the list. Note that if [List.length vs > capacity b], then only
      the last [capacity b] elements of the list remain in [b] at the end. *)
  val add_list : 'a t -> 'a list -> unit

  (** [clear b] removes all values from the buffer [b]. *)
  val clear : 'a t -> unit

  (** [fold b ~init ~f] folds over the value of the buffer [b], oldest to
      newest. *)
  val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b

  (** [elements b] is a list that contains the same elements as the buffer [b],
      oldest first, newest last. *)
  val elements : 'a t -> 'a list

end

module type HS = sig

   (** A subset of [Hashtbl.S]. *)

    type key
    type 'a t
    val create: int -> 'a t
    val remove: 'a t -> key -> unit
    val find_opt: 'a t -> key -> 'a option
    val replace : 'a t -> key -> 'a -> unit
    val length: 'a t -> int
    val clear: 'a t -> unit
    val fold_v: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

end

module type TABLER = functor (H: Hashtbl.HashedType) -> (HS with type key = H.t)

(** # Caches

    Ringo is a cache library. Below are signatures for caches.

    A [CACHE_MAP] is a collection of kay-value bindings. A [CACHE_SET] is a
    simple value store.

*)


module type CACHE_MAP = sig

  (** A Mutable structure akin to a hash-table, but with a size bound. Note
      that, different caches have different policies towards the size bounds:
      some uphold the bound strictly, some treat the bound as a suggestion. In
      addition, some caches count their elements somewhat sloppily.

      In general, the caches of ringo are intended to be used in settings that
      do not require strict, by-the-number, extremely-predictable behaviors.

      See [Ringo] (or [Functors]) for more information. *)

  (** The type of keys on which values in the cache are indexed. *)
  type key

  (** The type of caches holding bindings from [key] to ['a] *)
  type 'a t

  (** [create n] creates a cache with a size-bound of [n]. Remember that the
      size-bound is not upheld strictly by all caches. *)
  val create : int -> 'a t

  (** [replace c k v] binds the key [k] to the value [v] in the cache [c]. This
      may or may not cause another binding to be removed from the cache,
      depending on the number of bindings already present in the cache [c], the
      size-bound of the cache [c], and the policy of the cache [c] towards its
      size-bound.

      If [k] is already bound to a value in [c], the previous binding disappears
      and is replaced by the new binding to [v].

      Note that in caches with a [Sloppy] accounting policy, the old binding is
      erased but may still count towards the size bound for some time. In other
      words: apart for size bound consideration, the following sequences of
      operations are indistinguishable:
      [replace c k v; replace c k u]
      [replace c k v; remove c k; replace c k u] *)
  val replace : 'a t -> key -> 'a -> unit

  (** [fold f c init] folds the function [f] and value [init] over the bindings
      of [c].

      Note that for caches with a [Weak] overflow policy, this function may fold
      over a subset of the bindings of [c]. See [Ringo] (or [Functors]) for more
      details. *)
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [fold_v f c init] folds the function [f] and value [init] over the
      values held by the bindings of [c].

      It is less powerful than [fold] in that it does not grant access to the
      bindings' keys, but it does fold over all the values bound in [c], even
      when the [c] has a [Weak] overflow policy. *)
  val fold_v : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (** [find_opt c k] is [Some v] if [k] is bound to [v] in [c]. It is [None]
      otherwise.

      Note that the in caches with a non-[FIFO] replacement policy, this may
      have a side effect on the [k]-to-[v] binding. Specifically, in those
      caches, it might make it less likely to be removed when supernumerary
      bindings are inserted. *)
  val find_opt : 'a t -> key -> 'a option

  (** [remove c k] removes the binding from [k] in [c]. If [k] is not bound in
      [c], it does nothing.

      Note that in caches with a [Sloppy] accounting policy, removed bindings
      can still count towards the size bound for some time. *)
  val remove : 'a t -> key -> unit

  (** [length c] is the number of bindings held by [c]. *)
  val length : 'a t -> int

  (** [capacity c] is the number of bindings [c] can hold:
      [capacity (create n) = n] *)
  val capacity : 'a t -> int

  (** [clear c] removes all bindings from [c]. *)
  val clear : 'a t -> unit

  module H: Hashtbl.HashedType with type t = key

end

module type CACHE_SET = sig

  (** A Mutable structure akin to a set, but with a size bound. Note that,
      different caches have different policies towards the size bounds: some
      uphold the bound strictly, some treat the bound as a suggestion. In
      addition, some caches count their elements somewhat sloppily.

      In general, the caches of ringo are intended to be used in settings that
      do not require strict, by-the-number, extremely-predictable behaviors.

      See [Ringo] (or [Functors]) for more information. *)

  (** The type of values held by the cache. *)
  type elt

  (** The type of caches holding values of type [elt]. *)
  type t

  (** [create n] creates a unit-cache with a size-bound of [n]. Remember that
      the size-bound is not upheld strictly by all caches. *)
  val create : int -> t

  (** [add c v] adds the value [v] to the cache [c]. This may or may not cause
      another element to be removed from the cache, depending on the number of
      elements already present in the cache [c], the size-bound of the cache
      [c], and the policy of the cache [c] towards its size-bound.

      If [v] is already present in [c], the element may count twice towards the
      size bound for some time. In other words: apart for size bound in some
      cases, the following sequences of operations are indistinguishable:
      [add c v; add c u] and [add c v; remove c v; add c u] *)
  val add : t -> elt -> unit

  (** [fold f c init] folds the function [f] and value [init] over the elements
      of [c].

      Note that for caches with a [Weak] overflow policy, this function may fold
      over a subset of the elements of [c]. See [Ringo] (or [Functors]) for more
      details. *)
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  (** [mem c v] is [true] if [v] is present in [c]. It is [false] otherwise.

      Note that the in caches with a non-[FIFO] replacement policy, this may
      have a side effect on the [v] element. Specifically, in those caches, it
      might make it less likely to be removed when supernumerary elements are
      inserted. *)
  val mem : t -> elt -> bool

  (** [remove c v] removes the element [v] from [c]. If [v] is not present in
      [c], it does nothing.

      Note that in caches with a [Sloppy] accounting policy, removed elements
      can still count towards the size bound for some time. *)
  val remove : t -> elt -> unit

  (** [length c] is the number of elements present in [c]. *)
  val length : t -> int

  (** [capacity c] is the number of bindings [c] can hold:
      [capacity (create n) = n] *)
  val capacity : t -> int

  (** [clear c] removes all elements from [c]. *)
  val clear : t -> unit

end
