open! Core
open! Async

type 'a t

val create
  :  hashable: 'a Hashtbl.Hashable.t
  -> ?on_exn:('a -> exn -> unit)
  -> ?max_total_concurrent_jobs:int
  -> unit
  -> 'a t

val enqueue : 'a t -> key:'a -> (unit -> 'b Deferred.t) -> 'b Deferred.t

  (* [has_empty_spot t ~key] will be true if a job enqueued in [t] for [key] would run
     without waiting for any other jobs to complete. *)
val has_empty_spot : 'a t -> key:'a -> bool

val run_now
  :  'a t
  -> key:'a
  -> (unit -> 'b Deferred.t)
  -> [ `no_empty_spots | `running of 'b Deferred.t ]
