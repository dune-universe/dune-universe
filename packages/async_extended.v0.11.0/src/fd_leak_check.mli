open! Async

val percent_fds_in_use : unit -> float Deferred.t

val disable_periodic_check : unit -> unit

val enable_periodic_check : unit -> unit
