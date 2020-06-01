open Core_kernel

module Rpc = Async_rpc_kernel.Persistent_connection.Make (struct
    module Address = Host_and_port

    type t = Rpc.Connection.t

    let close t = Rpc.Connection.close t
    let is_closed t = Rpc.Connection.is_closed t
    let close_finished t = Rpc.Connection.close_finished t
  end)
