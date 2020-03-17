open Lambda_streams

(** Gets the first value from a lambda stream and turns it into a lwt promise *)
val first_to_lwt : 'a Finite.Async.t -> 'a Lwt.t

(** Gets the last value from a lambda stream and turns it into a lwt promise *)
val last_to_lwt : 'a Finite.Async.t -> 'a Lwt.t

val to_lwt_list : 'a Finite.Async.t -> 'a list Lwt.t

val from_lwt : 'a Lwt.t -> 'a Finite.Async.t

val to_lwt_stream : 'a Finite.Async.t -> 'a Lwt_stream.t

val from_lwt_stream : 'a Lwt_stream.t -> 'a Finite.Async.t
