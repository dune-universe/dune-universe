open Core
open Async

module type Rpc_resource = sig
  type t

  val connect : Host_and_port.t -> t Deferred.Or_error.t
  val to_rpc_connection : t -> Rpc.Connection.t
end

module Resource = struct
  type 'a t =
    { close_started  : unit Ivar.t
    ; resource       : 'a
    ; address        : Host_and_port.t
    } [@@deriving fields]
end

module Make(R : Rpc_resource) = struct
  module Key         = Host_and_port
  module Common_args = Unit

  type t = R.t Resource.t

  let open_ address () =
    let close_started = Ivar.create () in
    R.connect address
    >>|? fun resource ->
    { Resource.close_started; resource; address }
  ;;

  let rpc_connection t = R.to_rpc_connection (Resource.resource t)

  let close_finished t = Rpc.Connection.close_finished (rpc_connection t)

  let is_closed t =
    Ivar.is_full (Resource.close_started t) || Deferred.is_determined (close_finished t)
  ;;

  let close t =
    Ivar.fill_if_empty (Resource.close_started t) ();
    Rpc.Connection.close (rpc_connection t)
  ;;
end
