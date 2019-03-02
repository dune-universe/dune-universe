open! Core_kernel
open! Async_kernel
open! Import

module type Rpc_resource = sig
  type t

  val connect : Host_and_port.t -> t Deferred.Or_error.t
  val to_rpc_connection : t -> Rpc.Connection.t
end

type 'a t

val resource : 'a t -> 'a
val address : _ t -> Host_and_port.t

module Make (R : Rpc_resource) :
  Resource.S
  with module Key = Host_and_port
   and module Common_args = Unit
   and type t = R.t t
