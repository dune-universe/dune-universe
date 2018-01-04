open! Core
open! Async

(* [create ~start_on ~check_interval ~change_interval on_failure] starts a
   pair of async events and a thread that has expectations about those events.
   The entire program will be killed if the expectations of the watching thread
   are not met by async.

    [start_on]
        if set, the thread will not be started until the deferred passed in
        to start_on is set 
    [check_interval]
        how often the thread wakes up to check the world
    [change_interval]
        how often async tries to change the world
    [on_failure]
        what gets run when the world fails to change
*)
val create :
  ?start_on:unit Deferred.t
  -> check_interval:Time.Span.t
  -> change_interval:Time.Span.t
  -> (unit -> unit)
  -> unit
