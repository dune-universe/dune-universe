open Core_kernel
open Import

module Rpc : Rpc_kernel.Persistent_connection.S
  with type conn    = Rpc.Connection.t
   and type address = Host_and_port.t
