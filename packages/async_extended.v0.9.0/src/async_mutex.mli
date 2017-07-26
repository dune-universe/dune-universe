(** An async interface to the usual notion of mutex, in particular where [lock] returns
    a [unit Deferred.t] rather than blocking.

    Mutexes are almost never needed in async, which is based on cooperative threading, and
    thus one can exercise control over interleaving simply by placement of
    [Deferred.bind].

    Rather than use a mutex, one common idiom is to use a [Sequencer] to guarantee one
    client at a time has access to a shared piece of state. *)

open! Core
open! Async

type t

val create : unit -> t

(** [lock t] returns a deferred that is fulfilled when the caller has acquired the mutex.

    If the caller already holds the mutex, the deferred is never fulfilled.
 **)
val lock : t -> unit Deferred.t

val try_lock : t -> [ `Acquired | `Not_acquired ]

val unlock : t -> unit

val resource : t -> (unit, Nothing.t) Resource.t
