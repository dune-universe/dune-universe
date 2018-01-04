open! Core
open! Async



module Environment : sig
  type t = {
    connection_to_upstream_server : Rpc.Connection.t;
    connection_to_client : Rpc.Connection.t;
  }
end

(* Implements the given Rpc protocol by forwarding queries to the [Rpc.Connection.t]
   obtained by calling the provided function for every query. *)
val proxy : Rpc.Any.t -> Environment.t Rpc.Implementation.t
