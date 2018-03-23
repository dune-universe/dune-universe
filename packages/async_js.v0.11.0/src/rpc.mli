open Core_kernel
open Async_kernel
open Ocaml_uri
open Import

module Any             = Rpc_kernel.Rpc.Any
module Description     = Rpc_kernel.Rpc.Description
module Implementation  = Rpc_kernel.Rpc.Implementation
module Implementations = Rpc_kernel.Rpc.Implementations
module One_way         = Rpc_kernel.Rpc.One_way
module Pipe_rpc        = Rpc_kernel.Rpc.Pipe_rpc
module Rpc             = Rpc_kernel.Rpc.Rpc
module State_rpc       = Rpc_kernel.Rpc.State_rpc

module Pipe_close_reason = Rpc_kernel.Rpc.Pipe_close_reason

module Connection : sig
  include module type of struct include Rpc_kernel.Rpc.Connection end

  (** This type of client connects to the websocket at the root of some host and port,
      i.e. [ws://<address>/]. *)
  type ('rest, 'implementations) client_t
    =  ?uri              : Uri.t
    -> ?heartbeat_config : Heartbeat_config.t
    -> ?description      : Info.t
    -> ?implementations  : 'implementations Client_implementations.t
    -> 'rest

  val client     : (unit -> t Deferred.Or_error.t, 's) client_t
  val client_exn : (unit -> t Deferred.t         , 's) client_t
end
