open! Core
open! Async
open! Import

module type Heartbeat_streamable_state_rpc = sig
  val server : min_version:int -> max_version:int -> _ Rpc.Implementation.t list
  val client : version:int -> (int, unit, unit) Streamable.State_rpc.t
end
