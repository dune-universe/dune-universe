open! Core
open! Async

include
  module type of Httpaf
    with module Body := Httpaf.Body
     and type Version.t = Httpaf.Version.t
     and type Headers.t = Httpaf.Headers.t
     and type Request.t = Httpaf.Request.t

module Body : sig
  include module type of Httpaf.Body with type 'a t = 'a Httpaf.Body.t

  val to_string : [ `read ] t -> string Deferred.t
end

module Server : sig
  type t = Tcp.Server.inet

  module Response_body : sig
    type t = String of string | Pipe of string Pipe.Reader.t
  end

  type response = Response.t * Response_body.t

  type request_handler =
    body:[ `read ] Body.t ->
    Socket.Address.Inet.t ->
    Request.t ->
    response Deferred.t

  val create :
    on_handler_error:(Socket.Address.Inet.t -> Server_connection.error_handler) ->
    Tcp.Where_to_listen.inet ->
    request_handler ->
    t Deferred.t

  val respond_string :
    ?headers:Headers.t -> ?status:Status.t -> string -> response Deferred.t

  val respond_with_redirect : ?headers:Headers.t -> Uri.t -> response Deferred.t

  val respond_with_pipe :
    ?headers:Headers.t ->
    ?status:Status.t ->
    string Pipe.Reader.t ->
    response Deferred.t

  val respond_with_file :
    ?headers:Headers.t -> ?status:Status.t -> string -> response Deferred.t
end

module Cookie = Cookie
module Accept = Accept
