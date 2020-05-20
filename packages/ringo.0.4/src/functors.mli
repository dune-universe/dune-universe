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

(** [Make_map(C)(T)(H)] is a [Sigs.CACHE_MAP]. The exact behavior of the cache
    is determined by [C] and [T]. Specifically, the replacement policy of the
    cache is determined by [C] (see available collections bellow) and the
    strength of the size-bound is determined by [T] (see available tablers
    bellow). *)
module Make_map
  (Collection: Sigs.COLLECTION)
  (Tabler: Sigs.TABLER)
  (H: Hashtbl.HashedType)
: Sigs.CACHE_MAP with type key = H.t

(** [Make_set(C)(T)(H)] is a [Sigs.CACHE_SET]. The exact behavior of the cache
    is determined by [C] and [T]. Specifically, the replacement policy of the
    cache is determined by [C] (see available collections bellow) and the
    strength of the size-bound is determined by [T] (see available tablers
    bellow). *)
module Make_set
  (Collection: Sigs.COLLECTION)
  (Tabler: Sigs.TABLER)
  (H: Hashtbl.HashedType)
: Sigs.CACHE_SET with type elt = H.t

(** [LRU_Collection] is a [COLLECTION] meant to be used as an argument to
    [Make]. The replacement policy of caches instantiated with [LRU_Collection]
    is LRU: Least-Recently Used. Meaning that when inserting a supernumerary
    binding, the binding that is dropped is the least-recently
    inserted-or-found. In other words, the `find_opt` operation in these caches
    refreshes the associated binding.

    In addition, caches instantiated with [LRU_Collection] count their bindings
    precisely, meaning that [remove]d bindings do not count towards the
    size-bound. *)
module LRU_Collection: Sigs.COLLECTION

(** [FIFO_Sloppy_Collection] is a [COLLECTION] meant to be used as an argument to
    [Make]. The replacement policy of caches instantiated with
    [FIFO_Sloppy_Collection] is FIFO: First In First Out. Meaning that when
    inserting a supernumerary binding, the binding that is dropped is the
    least-recently inserted. In other words, the `find_opt` operation in these
    caches does not refreshes the associated binding.

    In addition, caches instantiated with [FIFO_Sloppy_Collection] count their
    bindings sloppily, meaning that [remove]d bindings count towards the
    size-bound until enough elements have been inserted that the removed binding
    disappears. *)
module FIFO_Sloppy_Collection: Sigs.COLLECTION

(** [FIFO_Precise_Collection] is a [COLLECTION] meant to be used as an argument
    to [Make]. The replacement policy of caches instantiated with
    [FIFO_Precise_Collection] is FIFO: First In First Out. Meaning that when
    inserting a supernumerary binding, the binding that is dropped is the
    least-recently inserted. In other words, the `find_opt` operation in these
    caches does not refreshes the associated binding.

    In addition, caches instantiated with [FIFO_Precise_Collection] count their
    bindings precisely, meaning that [remove]d bindings do not count towards the
    size-bound. *)
module FIFO_Precise_Collection: Sigs.COLLECTION

(** [Strong_tabler] is a [TABLER] meant to be used as an argument to [Make]. The
    size-bound policy of caches instantiated with [Strong_tabler] is strong.
    Meaning that old bindings are removed as soon as one supernumerary binding
    is inserted.

    If the memory size of keys and elements of such a cache are bounded, then
    the memory size of the whole cache is bounded in size.

    In addition, the [fold] and [iter] functions of such a cache traverse all of
    the bindings of the cache. *)
module Strong_tabler: Sigs.TABLER

(** [Weak_tabler] is a [TABLER] meant to be used as an argument to [Make]. The
    size-bound policy of caches instantiated with [Weak_tabler] is weak.
    Meaning that old bindings are not removed as soon as one supernumerary
    binding is inserted. Instead, such an old binding is held weakly by the
    cache. Weakly held elements can be collected by the Garbage Collector
    whenever memory is needed.

    In addition, the [fold] and [iter] functions of such a cache traverse only
    the strongly-held bindings of the cache. The [length] function counts both
    weakly- and strongly-held bindings. *)
module Weak_tabler: Sigs.TABLER

(**/**)
module Unbox (C: Sigs.COLLECTION_BARE) : Sigs.UNBOXED_COLLECTION
