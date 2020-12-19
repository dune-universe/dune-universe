open! Core
open! Async
open Reddit_api_kernel

module Credentials : sig
  type t =
    { client_id : string
    ; client_secret : string
    ; password : string
    ; username : string
    }
  [@@deriving sexp]
end

type t [@@deriving sexp_of]

val create : Credentials.t -> user_agent:string -> t
val call : t -> 'a Api.t -> ('a, Api.Api_error.t) Result.t Deferred.t
val call_exn : t -> 'a Api.t -> 'a Deferred.t

val call_raw
  :  t
  -> 'a Api.t
  -> (Cohttp.Response.t * Cohttp.Body.t, Exn.t) Result.t Deferred.t

module Remote : sig
  (** Any connection can be turned into an RPC server, acting as a shared
   *  connection for multiple client [Connection.t]s. Rate limiting is managed
   *  on the server side. *)

  val serve
    :  t
    -> where_to_listen:(([< Socket.Address.t ] as 'a), 'b) Tcp.Where_to_listen.t
    -> ('a, 'b) Tcp.Server.t Deferred.t

  val connect_exn : [< Socket.Address.t ] Tcp.Where_to_connect.t -> t Deferred.t
end

module For_testing : sig
  val with_cassette
    :  Filename.t
    -> credentials:Credentials.t
    -> f:(t -> 'a Deferred.t)
    -> 'a Deferred.t
end
