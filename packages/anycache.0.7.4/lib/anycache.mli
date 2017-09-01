(******************************************************************************)
(* Copyright (c) 2014-2016 Skylable Ltd. <info-copyright@skylable.com>        *)
(*                                                                            *)
(* Permission to use, copy, modify, and/or distribute this software for any   *)
(* purpose with or without fee is hereby granted, provided that the above     *)
(* copyright notice and this permission notice appear in all copies.          *)
(*                                                                            *)
(* THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES   *)
(* WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF           *)
(* MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR    *)
(* ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES     *)
(* WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN      *)
(* ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF    *)
(* OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.             *)
(******************************************************************************)

(** Anycache LRU/2Q cache

    {e v0.7.4 — {{:https://gitlab.com/edwintorok/ocaml-anycache }homepage}}

    Consult the {{!basics}basics}, {{!examples}examples}, and
    {{!Anycache.S}module documentation}.

    {3 References}
    {ul
    {- T. Johnson and D. Shasha 
    {e {{:http://www.vldb.org/conf/1994/P439.PDF} 2Q: A Low Overhead
    High Performance Buffer Management Replacement Algorithm}, 1994}}
    {- T. Unagst
    {e {{:http://www.tedunangst.com/flak/post/2Q-buffer-cache-algorithm}
    2Q buffer cache algorithm}, 2014}}}
*) 
(** {1 Anycache} *)

module type Monad = sig
  type +'a t
  (** the type for deferred computation with result of type ['a] *)

  type ('a, 'b) result = Ok of 'a | Error of 'b
  (** result type for backward compatibility *)

  val return : 'a -> 'a t
  (** a successfull result *)

  val fail : exn -> 'a t
  (** an error *)

  val ( >>? ) : 'a t -> (('a, exn) result -> 'b t) -> 'b t
  (** [v >>= g] chains the computation of [v] and the function [g].
      When the computation of [v] finishes [g] is invoked with its result,
      or error.
      This allows to post-process the result or perform error handling.*)
end

module type S = sig
  type key
  (** the type for cache keys *)

  type 'a deferred
  (** the type for deferred computations *)

  type 'a t
  (** the type for caches storing associating
      keys of type [key] with values of type ['a] *)

  type 'a validator = (key * 'a option) -> 'a deferred
  (** the type for cache validation callbacks.
      [validate (key,old)] is called with [old = None] if the key is not in the
      cache yet and has to be computed.
      It is called with [old=Some data] if the key is already in the cache.
      The validator has to decide whether to return the same value or
      recompute it (perhas because it is expired / no longer fresh).
  *)

  val create : int -> 'a t
  (** [create n] creates a LRU/2Q that can hold at most [n] elements *)

  val with_cache : 'a t -> (key -> 'a deferred) -> key -> 'a deferred
  (** [with_cache cache f key] acts like [f key], except it might make
      fewer calls to [f] when the [key] is already in the cache.
      On successful termination of [f key] the result is stored in the cache.
      [f key] must always return the same value when called with the same key.
      Cached values never expire, they are only removed if the cache capacity
      is exceeded, see {{!lrupolicy}cache replacement policy}.
  *)

  val with_validator : 'a t -> 'a validator -> key -> 'a deferred
  (** [with_validator cache validator key] calls [validator] each time [key] is
      accessed.
      If the [key] is not in the cache [validator (key, None)] is called and
      it should compute the value associated with [key].
      If the [key] is already in the cache then [validator [key, Some value]]
      is called and [validator] should decide whether the [value] is still fresh,
      or if it should be recomputed.
      The value returned by [validator] in either case is stored in the [cache].
      [validate (key,_)] is allowed to return different values when called with
      the same key again, provided it properly validates the lifetime of old
      entries.
      Values are removed from the cache when its capacity is exceeded, see
      {{!lrupolicy}cache replacement policy}.
  *)

  val get : 'a t -> key -> 'a option deferred
  (** [get cache key] retrieves the [key] from the [cache] if it exists.
      The key's position in LRU is updated *)

  val set : 'a t -> key -> 'a -> unit
  (** [set cache key value] stores [value] under [key] in the [cache].
      If a value for [key] was already stored then it is replaced by the
      new [value].
      It is not specified whether the key's position is updated in the LRU.*)
end

(** define a cache with custom key and monad types *)
module Make(K: Map.OrderedType)(M: Monad) : S with
  type key = K.t and type 'a deferred = 'a M.t

(** a direct computation that can either succeed or fail *)
module Direct : Monad with type 'a t = ('a, exn) Result.result and
type ('a, 'b) result = ('a, 'b) Result.result

(** a cache with string keys and {!Result.result} values *)
include S with type key = string and type 'a deferred = ('a, exn) Result.result

module PendingLimit : sig
  module Make :
    functor (Key : Map.OrderedType) ->
    functor (M : Types.Monad) ->
    sig
      type 'a t
      val create : unit -> 'a t
      val bind : 'a t -> Key.t -> (Key.t -> 'a M.t) -> 'a M.t
    end
end

(** {1:basics Basics}

    This module defines a cache that can be used to speed up slow computations
    (typically involving network or disk I/O).
    When a computation finishes successfully its result is stored in the cache.
    If the computation fails only the key is cached, the value is recomputed
    assuming that errors are transitory.

    If you try to lookup a value in the cache while a computation is in progress
    (only possible if you are using Lwt or Async) then you get the result
    from the previous computation if it succeeds (and recomputed on failure).
    This ensures that you don't flood the disk/network with queries for the same
    key. Handling timeouts is the caller's responsibility though.

    If the computation is referentially transparent
    (always returns the same value when invoked with the same key) then
    {!Anycache.S.with_cache} should be used for caching.

    If the computation can return different values when called at
    different times, and this affects the semantics of your application
    (e.g. reading a file, performing an http query) then
    {!Anycache.S.with_validator} should be used.
    It allows to specify a validator that can determine whether the result is
    still valid, and recompute it if needed
    (e.g. by performing a conditional HTTP GET).

    The lookup times are worst case logarithmic in the size of the cache.
    This is needed to avoid the worst case linear lookup times of a hash table
    in case of collisions (the keys are assumed to be externally controllable,
    e.g. from a URL or form values).
    If this overhead is not acceptable or you just want to cache pure computations
    look at [Memo] (from [core_kernel]) or [CCCache] (from [containers])
    instead.

    A cache with string keys is provided by default, you can instantiate your
    own using {!Make}.

    {2:lrupolicy Cache replacement policy}

    When the cache's capacity is exceeded an old element is removed.
    The old element is determined such that we keep often accessed values in
    the cache, and that accessing a long sequence of elements once (a scan)
    doesn't completely remove all elements from the cache.

    Three queues are used: short-term ([A1in]),
    evicted short-term ([A1out]), and long-term ([Amain]).

    Elements start out in the short-term cache, and when its capacity is exceeded
    the element is moved to the evicted short-term queue (we preserve the data).
    If it is accessed again while on the evicted queue it is promoted to the
    long-term queue, otherwise when the evicted queue's capacity is exceeded
    it is dropped.
    Accessing elements while on the short-term queue has no effect,
    and it is managed like a FIFO queue.

    When the long-term queue's capacity is exceeded its Least Recently Used
    element is discarded.

    The short-term queue stores at most 25% of total capacity, and the long-term
    queue stores at most 50%.
    The evicted queue's size is dynamically adjusted to use all available space,
    i.e. when the long-term queue has few elements it will store more than 25%
    of the elements.
    This is done so that we always use the cache to its full capacity, even if
    the long-term elements are few.
*)

(** {1:examples Examples}

    Given a potentially slow function (DNS lookup):
    {[
      open Result
      let lookup name =
        Printf.printf "Looking up %s\n" name;
        try Ok (Unix.getaddrinfo name "" [])
        with e -> Error e

      let print_result = function
      | Ok lst -> Printf.printf "Got %d addresses\n" (List.length lst);
      | Error e -> raise e
    ]}

    We can construct a cached version:

    {[
      let cache = Anycache.create 1024
      let cached_lookup = Anycache.with_cache cache lookup

      let () =
        print_result (cached_lookup "example.com");
        print_result (cached_lookup "example.com");
        print_result (cached_lookup "example.net");
    ]}

    Running it produces:
    {v
$ ocaml $(opam config var anycache:doc)/example.ml
Looking up example.com
Got 6 addresses
Got 6 addresses
Looking up example.net
Got 6 addresses
v}
*)
