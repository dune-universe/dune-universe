open! Core
open! Async

(* A Signal is a broadcast communication mechanism useful for event-based and
functional reactive programming that combines Synccasts with duplicate handling
and a notion of "current value" *)
type 'a t

(* [create ?eq s] returns a new Signal with the initial value s.  If eq is
  defined then it will be used to filter out duplicate signals *)
val create : ?eq:('a -> 'a -> bool) -> 'a -> 'a t

(* [close ()] closes the signal to further updates *)
val close : 'a t -> unit

(* [get t] returns the current value of t *)
val get : 'a t -> 'a

(* [set t v] sets the current value of the Signal *)
val set : 'a t -> 'a -> unit

(* [send t v] alias for set *)
val send : 'a t -> 'a -> unit

(* [upon t ~f:f] runs f on the next update of t *)
val upon : 'a t -> f:('a -> unit) -> unit

(** [next t] returns the next value in t *)
val next : 'a t -> 'a Deferred.t

(* [on_update t f] registers a listener with the signal that listens to all
  subsequently sent messages *)
val on_update : 'a t -> ?stop_condition:('a -> bool) -> ('a -> unit) -> unit

(* [register t ~f:f] registers a function that is called on every update of t *)
val register : 'a t -> f:('a -> Synccast.interest_status Deferred.t) -> unit

(* [register_init t ~f:f ~i:i] registers f and runs i at the time of registration.
  i completes before the signal continues *)
val register_init : 'a t
  -> init:(unit -> unit Deferred.t)
  -> f:('a -> Synccast.interest_status Deferred.t)
  -> unit

(** [register_self_init t ~f] same as register init, but uses the function f as the
  initialization function *)
val register_self_init : 'a t -> f:('a -> Synccast.interest_status Deferred.t) -> unit

(* [create_repeater ?randomize:r ?stop_condition:s ()] returns a signal that is
updated every span.  If r is given then the spacing of the updates will be
randomized.  If s is given then s will be called on every update and if s
returns true no more updates will be sent and the repeater will be closed. *)
val create_repeater :
  ?randomize:float ->
  ?stop_condition:(Time.t -> bool) ->
  Time.Span.t ->
  Time.t t
