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

(** Preamble *)

module type UNBOXED_COLLECTION = Sigs.UNBOXED_COLLECTION

(** [Ring] is a potentially useful module that is used internally to manage
    bounded, FIFO collections of items. The documentation is available in
    {!UNBOXED_COLLECTION}.

    It is implemented as an abstraction over an array. *)
module Ring : UNBOXED_COLLECTION

(** [Dll] is a potentially useful module that is used internally to manage
    bounded, LRU collections of items. The documentation is available in
    {!UNBOXED_COLLECTION}.

    It is implemented as an abstraction over a doubly-linked list.

    The implementations of [Ring] and [Dll] are functionally indistinguishable.
    However, their memory consumption differs. On the one hand, with a [Ring],
    the whole structure is allocated in its entirety as soon as a single element
    is [add]ed. Afterwards, there are no more allocations.

    On the other hand, with a [Dll], cells holding the [add]ed values are
    allocated on a by-need basis. Inserting a supernumerary element renders one
    single cell garbage-collectible.

    In other words, [Ring] allocates a bigger chunk of data in one go but is
    stable afterwards, whereas [Dll] allocates small chunks of data one-by-one.
    *)
module Dll : UNBOXED_COLLECTION


(** Caches *)

module type CACHE_MAP = Sigs.CACHE_MAP
module type CACHE_SET = Sigs.CACHE_SET

(** All caches of Ringo have either the {!CACHE_MAP} interface (for key-value
    stores) or the {!CACHE_SET} interface (for value stores). However, their
    behavior can be tweaked by the parameters below. *)

(** [replacement] is for defining the replacement policy of a cache. [LRU] is
    for "Least Recently Used", meaning that when a supernumerary item is
    inserted in the cache, the least recently used item is removed to make room.
    [FIFO] is for "First-In, First-Out" meaning that when a supernumerary item
    is inserted in the cache, the oldest inserted element is removed to make
    room. *)
type replacement =
  | LRU
  | FIFO

(** [overflow] is for defining the overflow policy of a cache. [Strict] means
    that the cache never holds more element than is specified when calling
    [create] (see {!MAP_MAKER} and {!SET_MAKER} below and {!CACHE_MAP} and
    {!CACHE_SET}). [Weak] means that the cache may hold more elements than
    specified when calling [create] but that supernumerary elements may be
    collected by the Garbage Collector. *)
type overflow =
  | Strict
  | Weak

(** [accounting] is for defining the accounting policy of a cache. [Precise]
    means that the cache counts its number of elements precisely. [Sloppy] means
    that the cache may count elements that have been [remove]d from the cache as
    still being held by the cache.

    Note that when requesting a [Sloppy] cache, the library might give you a
    [Precise] cache if there is no additional runtime cost. In general, [Sloppy]
    caches are more efficient, but (1) they are not adapted to situations where
    you remove many elements and (2) depending on the other parameters they
    might be only as-efficient-as (not more) than [Precise] caches.

    Use [Precise] only if you use [remove] a lot or if you need strong
    guarantee on the number of elements. *)
type accounting =
  | Precise
  | Sloppy

(** A [MAP_MAKER] is a functor that instantiates [CACHE_MAP]s based on a given type and
    its associated hash function. *)
module type MAP_MAKER = functor (H: Hashtbl.HashedType) -> CACHE_MAP with type key = H.t
type map_maker = (module MAP_MAKER)

(** [map_maker ~replacement ~overflow ~accounting] is a first-class [MAP_MAKER] that
    instantiates caches with the policies specified by [replacement],
    [overflow], and [accounting]. *)
val map_maker : replacement:replacement -> overflow:overflow -> accounting:accounting -> map_maker


(** A [SET_MAKER] is a functor that instantiates [CACHE_SET]s based on a given
    type and its associated hash function. *)
module type SET_MAKER = functor (H: Hashtbl.HashedType) -> CACHE_SET with type elt = H.t
type set_maker = (module SET_MAKER)

(** [set_maker ~replacement ~overflow ~accounting] is a first-class
    [SET_MAKER] that instantiates caches with the policies specified by
    [replacement], [overflow], and [accounting]. *)
val set_maker : replacement:replacement -> overflow:overflow -> accounting:accounting -> set_maker
