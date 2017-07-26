open! Core
open! Async

(* a synccast is a multicast stream where each event is fully processed by all listeners
before any processing of subsequent events begins *)
type 'a t

(* all registered listeners must return a status indicating their continued
interest (or lack of interest) in future messages *)
type interest_status = [`Continue | `Leave]

(* [create ()] returns a new empty Synccast stream *)
val create : unit -> 'a t

(* [close ()] closes the synccast to further updates *)
val close : 'a t -> unit

(* [register t f] registers a listener with the stream that will get
  all subsequently sent messages until it returns Leave *)
val register: 'a t -> f:('a -> interest_status Deferred.t) -> unit

(* [register_init t ~i ~f] registers a listener f with the stream in the same manner
  as register, except that i will be run with the last value in the stream at the
  time of the registration *)
val register_init : 'a t
  -> i:(unit -> unit Deferred.t)
  -> f:('a -> interest_status Deferred.t)
  -> unit

(* [on_update t f] registers a listener with the behavior that listens to all
  subsequently sent messages *)
val on_update : 'a t -> f:('a -> unit) -> unit

(* [upon t f] registers a listener with the stream that reacts to the next event and
  then stops listening *)
val upon: 'a t -> f:('a -> unit) -> unit

(* [send t v] sends a value to the synccast.  Will raise Closed if the
   synccast is closed. *)
val send : 'a t -> 'a -> unit

(** [next t] returns the next value in the synccast *)
val next : 'a t -> 'a Deferred.t