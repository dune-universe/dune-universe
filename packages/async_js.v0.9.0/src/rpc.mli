open Core_kernel
open Async_kernel
open Import

module Any             = Rpc_kernel.Any
module Description     = Rpc_kernel.Description
module Implementation  = Rpc_kernel.Implementation
module Implementations = Rpc_kernel.Implementations
module One_way         = Rpc_kernel.One_way
module Pipe_rpc        = Rpc_kernel.Pipe_rpc
module Rpc             = Rpc_kernel.Rpc
module State_rpc       = Rpc_kernel.State_rpc

module Pipe_close_reason = Rpc_kernel.Pipe_close_reason

module Connection : sig
  include module type of struct include Rpc_kernel.Connection end

  type ('a, 's) client_t
    = ?address           : Host_and_port.t (* Default: host and port of the current page *)
    -> ?heartbeat_config : Heartbeat_config.t
    -> ?description      : Info.t
    -> ?implementations  : 's Client_implementations.t
    -> unit
    -> 'a Deferred.t

  val client     : (t Or_error.t, 's) client_t
  val client_exn : (t           , 's) client_t
end
