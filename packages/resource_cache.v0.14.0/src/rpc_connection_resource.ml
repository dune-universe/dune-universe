open! Core_kernel
open! Async_kernel
open! Import

module type Rpc_resource = sig
  type t

  val connect : Host_and_port.t -> t Deferred.Or_error.t
  val to_rpc_connection : t -> Rpc.Connection.t
end

module T = struct
  type 'a t =
    { close_started : unit Ivar.t
    ; resource : 'a
    ; address : Host_and_port.t
    }
  [@@deriving fields]
end

include T

module Make (R : Rpc_resource) = struct
  module Key = Host_and_port
  module Common_args = Unit

  type t = R.t T.t

  let open_ address () =
    let close_started = Ivar.create () in
    let open Deferred.Or_error.Let_syntax in
    let%map resource = R.connect address in
    { T.close_started; resource; address }
  ;;

  let rpc_connection t = R.to_rpc_connection (T.resource t)
  let close_finished t = Rpc.Connection.close_finished (rpc_connection t)

  let has_close_started t =
    Ivar.is_full (T.close_started t) || Deferred.is_determined (close_finished t)
  ;;

  let close t =
    Ivar.fill_if_empty (T.close_started t) ();
    Rpc.Connection.close (rpc_connection t)
  ;;
end
