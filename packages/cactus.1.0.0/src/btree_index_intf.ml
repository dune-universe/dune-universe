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
  (** The type for indexes. *)

  type key
  (** The type for keys. *)

  type value
  (** The type for values. *)

  type cache
  (** The type for caches of index instances. *)

  val empty_cache : unit -> cache
  (** Construct a new empty cache of index instances. *)

  val v :
    ?flush_callback:(unit -> unit) ->
    ?cache:cache ->
    ?fresh:bool ->
    ?readonly:bool ->
    ?throttle:[ `Overcommit_memory | `Block_writes ] ->
    log_size:int ->
    string ->
    t
  (** The constructor for indexes.

      @param flush_callback A function to be called before any new bindings are persisted to disk
      (including both automatic flushing and explicit calls to {!flush} or {!close}).

      This can be used to ensure certain pre-conditions are met before bindings are persisted to
      disk. (For instance, if the index bindings are pointers into another data-structure [d], it
      may be necessary to flush [d] first to avoid creating dangling pointers.)
      @param cache used for instance sharing.
      @param fresh whether an existing index should be overwritten.
      @param read_only whether read-only mode is enabled for this index.
      @param throttle the strategy to use when the cache are full and and async in already in
      progress. [Block_writes] (the default) blocks any new writes until the merge is completed.
      [Overcommit_memory] does not block but continues to fill the (already full) cache.
      @param log_size the maximum number of bindings in the `log` IO. *)

  (* TODO : [cache] is basically a (string, btree) hashtbl which maps a path to the btree living there *)

  val clear : t -> unit
  (** [clear t] clears [t] so that there are no more bindings in it. *)

  val find : t -> key -> value
  (** [find t k] is the binding of [k] in [t]. *)

  val mem : t -> key -> bool
  (** [mem t k] is [true] iff [k] is bound in [t]. *)

  val replace : ?overcommit:bool -> t -> key -> value -> unit
  (** [replace t k v] binds [k] to [v] in [t], replacing any existing binding of [k].

      If [overcommit] is true, the operation does not triger a merge, even if the caches are full.
      By default [overcommit] is false. *)

  val filter : t -> (key * value -> bool) -> unit
  (** [filter t p] removes all the bindings (k, v) that do not satisfy [p]. This operation is costly
      and blocking. *)

  val iter : (key -> value -> unit) -> t -> unit
  (** Iterates over the index bindings. Limitations:

      - Order is not specified.
      - In case of recent replacements of existing values (since the last merge), this will hit both
        the new and old bindings.
      - May not observe recent concurrent updates to the index by other processes. *)

  val flush : ?no_callback:unit -> ?with_fsync:bool -> t -> unit
  (** Flushes all internal buffers of the [IO] instances.

      - Passing [~no_callback:()] disables calling the [flush_callback] passed to {!v}.
      - If [with_fsync] is [true], this also flushes the OS caches for each [IO] instance. *)

  val close : ?immediately:unit -> t -> unit
  (** Closes all resources used by [t], flushing any internal buffers in the instance.

      If [immediately] is passed, this operation will abort any ongoing background processes. This
      guarantees not to corrupt the store, but may require additional work to be done on the next
      startup. *)

  (* TODO : close must actually close the file handles if there are no more reference to the cache, otherwise decrease the reference counter. Conversely, [v] must bump a cache reference counter *)

  val sync : t -> unit
  (** [sync t] syncs a read-only index with the files on disk. Raises {!RW_not_allowed} if called by
      a read-write index. *)

  val is_merging : t -> bool
  (** [is_merging t] returns true if [t] is running a merge. Raises {!RO_not_allowed} if called by a
      read-only index. *)

  val merge : t -> unit
  (** [merge t] forces a merge for [t].

      If there is no merge running, this operation is non-blocking, i.e. it returns immediately,
      with the merge running concurrently.

      If a merge is running already, this operation blocks until the previous merge is complete. It
      then launches a merge (which runs concurrently) and returns. *)

  val try_merge : t -> unit
  (** [try_merge] is like {!merge} but is a no-op if the number of entries in the write-ahead log is
      smaller than [log_size]. *)
end

module type Key = sig
  type t [@@deriving repr]
  (** The type for keys. *)

  val equal : t -> t -> bool
  (** The equality function for keys. *)

  val hash : t -> int
  (** Note: Unevenly distributed hash functions may result in performance drops. *)

  val hash_size : int
  (** The number of bits necessary to encode the maximum output value of {!hash}. `Hashtbl.hash`
      uses 30 bits. Overestimating the [hash_size] will result in performance drops; underestimation
      will result in undefined behavior. *)

  val encode : t -> string
  (** [encode] is an encoding function. The resultant encoded values must have size {!encoded_size}. *)

  val encoded_size : int
  (** [encoded_size] is the size of the result of {!encode}, expressed in number of bytes. *)

  val decode : string -> int -> t
  (** [decode s off] is the decoded form of the encoded value at the offset [off] of string [s].
      Must satisfy [decode (encode t) 0 = t]. *)
end

module type Value = sig
  type t [@@deriving repr]

  val encode : t -> string

  val encoded_size : int

  val decode : string -> int -> t
end

module Stats = Index_stats

module type MAKER = functor (Key : Key) (Value : Value) ->
  S with type key = Key.t and type value = Value.t

module type Btree_index = sig
  module type S = S

  module Make : MAKER

  module Stats = Stats
end
