open Core_kernel
open Async_kernel

include module type of struct
  include Async_rpc_kernel.Rpc
end

module Connection : sig
  include module type of struct
    include Connection
  end

  (** This type of client connects to the websocket at the root of some host and port,
      i.e. [ws://<address>/]. *)
  type ('rest, 'implementations) client_t =
    ?uri:Uri.t
    -> ?heartbeat_config:Heartbeat_config.t
    -> ?description:Info.t
    -> ?implementations:'implementations Client_implementations.t
    -> 'rest

  val client : (unit -> t Deferred.Or_error.t, 's) client_t
  val client_exn : (unit -> t Deferred.t, 's) client_t
end
