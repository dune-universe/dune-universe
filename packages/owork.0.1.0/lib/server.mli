(** The type of the server *)
type t

val run : Config.t -> unit Lwt.t
(** Run the server. This sets up the socket file to listen on and handles connections. It runs until the server is stopped via a signal. *)
