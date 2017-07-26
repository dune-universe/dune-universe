open! Core
open! Async

type ('a, 'b) t

(* [add_to_cache] tells the deferred cache how to fetch values when [find] is called and
   there is no cached value. If this returns an error, that error is given to the [find]
   call which triggered [add_to_cache]

   [to_remove] tells the cache how to clean-up values before removing them from the
   cache. For example, when caching catalog cells, [to_remove] could be
   Subscriber.Cell.close.

   [on_exn] will be called on any exception raised inside the cache. Default behaviour
   is to swallow exceptions.

   If [cache_errors] is true, errors returned by [add_to_cache] (and given to the
   triggering [find]) will be cached. While an error is cached, calls of [find] will be
   given the cached error, rather than triggering [add_to_cache]. If [cache_errors] is
   false, then calling [find] will trigger [add_to_cache] every time it is called, so
   long as [add_to_cache] keeps returning errors. This defaults to false.

   [remove_if_unread_for] If a cached value goes unread (no calls of [find]) for this
   span of time, the cache will drop the cached value after calling [to_remove]. By
   default cached values are not removed.

   [max_cached_data_age] When a new value is added to the cache, the cache waits this
   span of time then removes the cached value. This is distinct from
   [remove_if_unread_for].  [remove_if_unread_for] will never drop a cached value if
   that value is being regularly read. [max_cached_data_age] on the other hand will drop
   old cached values no matter how frequently they are accessed. By default cached
   values are not removed.

   [max_jobs_across_all_keys] Limits the number of simultaneous jobs being executed on
   the sequencers inside the cache. In particular, this limits the number of
   [add_to_cache] calls being executed simultaneously. By default, there is no
   maximum. *)

val create
  :  add_to_cache : ('a -> 'b Or_error.t Deferred.t)
  -> to_remove : ('b -> unit Deferred.t)
  -> hashable: ('a Hashtbl.Hashable.t)
  -> ?on_exn : ('a -> exn -> unit)
  -> ?cache_errors: bool
  -> ?remove_if_unread_for: Time.Span.t
  -> ?max_cached_data_age: Time.Span.t
  -> ?max_total_concurrent_jobs : int
  -> unit
  -> ('a, 'b) t

val find : ('a, 'b) t -> key:'a -> 'b Or_error.t Deferred.t
val find_cached_only : ('a, 'b) t -> key:'a -> 'b option
val remove : ('a, _) t -> key:'a -> unit Deferred.t
