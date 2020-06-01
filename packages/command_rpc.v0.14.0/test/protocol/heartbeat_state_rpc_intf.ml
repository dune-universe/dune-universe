open! Core
open! Async
open! Import

module type Heartbeat_state_rpc = sig
  val server
    :  min_version:int
    -> max_version:int
    -> Command_rpc.Command.Invocation.t Rpc.Implementation.t list

  val client : version:int -> (int, unit, unit, Error.t) Rpc.State_rpc.t
end
