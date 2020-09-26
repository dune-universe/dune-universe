open Core_kernel

module Rpc :
  Async_rpc_kernel.Persistent_connection.S
  with type conn = Rpc.Connection.t
   and type address = Host_and_port.t
