open! Core
open Async
module Connection_close_reason = Connection_close_reason

type t

val sec_websocket_accept_header_value : sec_websocket_key:string -> string

module Websocket_role : sig
  type t =
    | Client
    | Server
  [@@deriving sexp_of]
end

val create
  :  ?opcode:[ `Text | `Binary ]
  -> role:Websocket_role.t
  -> Reader.t
  -> Writer.t
  -> t

val pipes : t -> string Pipe.Reader.t * string Pipe.Writer.t
val close_finished : t -> (Connection_close_reason.t * string * Info.t option) Deferred.t
