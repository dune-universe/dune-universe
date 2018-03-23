(** This is a simple tcp proxy that might be useful for simulating connection problems
    between clients and servers. *)

open! Core
open! Async

type t

val create : proxy_listen_port:int -> server_listen_port:int -> t Deferred.t

val proxy_listen_port : t -> int

val close : t -> unit Deferred.t

val stop_client_to_server : t -> unit
val stop_server_to_client : t -> unit

val allow_client_to_server : t -> unit
val allow_server_to_client : t -> unit
