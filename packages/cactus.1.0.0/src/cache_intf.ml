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

module type CALIFORNIA = sig
  (* You can (almost) never leave the california cache *)

  type key

  type value

  type t

  val v :
    flush:(key -> value -> unit) ->
    load:(?available:value -> key -> value) ->
    filter:(value -> [ `California | `Lru | `Volatile ]) ->
    int ->
    t
  (** The constructor for caches.

      @param flush The function to be called before any binding is discarded from the cache.
      @param load The (possibly costly) function to call to fetch a new binding into the cache. An
      optional argument [~available] can be passed to recycle memory allocations from another, never
      used again, value.
      @param filter A hierarchy function between bindings. [`California] is for most frequent
      bindings which should never be discarded. [`Lru] is for remaining frequent bindings that are
      too numerous to fit in memory. [`Volatile] is for the least frequent bindings, discarded soon
      after loading.

      [v ~flush ~load ~filter lru_cap] is a 3 level cache, with a bound of [lru_cap] bindings on the
      middle level. *)

  val find : t -> key -> value
  (** [find t k] looks for a key in the cache. If the key is not present in the cache it is read
      from disk (using [load]) and added to the cache (using [filter]). *)

  val reload : t -> key -> unit
  (** [reload t k] moves [k] between caches. Call this function after a node split to ensure that
      the newly created pages are in the right cache. *)

  val update_filter : t -> filter:(value -> [ `California | `Lru | `Volatile ]) -> unit
  (** [update_filter t filter] updates the filter and reapplies it on all the bindings in the
      california cache. It flushes and clears the lru cache. *)

  val release : t -> unit
  (** [release t] flushes and clears the volatile cache. *)

  val deallocate : t -> key -> unit

  val clear : t -> unit

  val flush : t -> unit

  val length : t -> int
  (** Number of bindings in the cache. *)
end

module type MAKER = functor
  (K : Hashtbl.HashedType)
  (V : sig
     type t
   end)
  -> CALIFORNIA with type key = K.t and type value = V.t

module type Cache = sig
  module Make : MAKER
end
