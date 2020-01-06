open Fswatch

val init_session : Monitor.t -> handle * Event.t array Lwt_mvar.t
val start_monitor : handle -> unit -> unit Lwt.t

