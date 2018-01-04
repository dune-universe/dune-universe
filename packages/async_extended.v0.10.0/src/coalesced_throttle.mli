(** A throttle that coalesces jobs, only fully executing the most recent job. *)
open! Async

(** A coalesced throttle. *)
type 'a t

(** Create a coalesced throttle. *)
val create : unit -> 'a t

(** Enqueue a job, which may be coalesced into a later job. *)
val enqueue :
  'a t
  -> (maybe_abort : (continue:(unit -> 'a Deferred.t) -> 'a Deferred.t)
         -> 'a Deferred.t)
  -> 'a Deferred.t
