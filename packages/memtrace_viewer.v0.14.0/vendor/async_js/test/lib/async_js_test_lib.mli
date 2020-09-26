open! Core_kernel
open! Async_rpc_kernel

module Rpcs : sig
  val send_string : (string, string) Rpc.Rpc.t
  val close_connection : (unit, unit) Rpc.Rpc.t
end

module Callback_function : sig
  type 'a t = Open_rpc_and_wait of 'a [@@deriving sexp_of]

  val name : unit t -> string
  val to_javascript_invocation : Uri.t t -> string
end
