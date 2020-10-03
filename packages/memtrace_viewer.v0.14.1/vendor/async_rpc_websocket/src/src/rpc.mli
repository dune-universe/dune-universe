open! Core
open! Async

module Connection_source : sig
  type 'a t =
    | Web of 'a
    | Plain_tcp
  [@@deriving sexp_of]
end

module Connection_initiated_from : sig
  type t =
    | Websocket_request of Cohttp_async.Request.t
    | Tcp
  [@@deriving sexp_of]
end

type http_handler =
  body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp_async.Request.t
  -> Cohttp_async.Server.response Deferred.t

type raw_http_handler =
  body:Cohttp_async.Body.t
  -> Socket.Address.Inet.t
  -> Cohttp_async.Request.t
  -> Cohttp_async.Server.response_action Deferred.t

type should_process_request =
  Socket.Address.Inet.t
  -> (Cohttp.Header.t * [ `is_websocket_request of bool ]) Connection_source.t
  -> unit Or_error.t


(** The ['c] argument allows a wrapper for [handler] to pass extra information to the
    [initial_connection_state] and [http_handler] handlers. This parameter is fixed to
    [unit] in the functions other than [handler]. *)
type ('s, 'c, 'r) common_args =
  implementations:'s Async.Rpc.Implementations.t
  -> initial_connection_state:
       ('c
        -> Connection_initiated_from.t
        -> Socket.Address.Inet.t
        -> Async.Rpc.Connection.t
        -> 's)
  -> ?http_handler:('c -> http_handler)
  (** [http_handler] describes how to handle non-websocket HTTP requests.
      Defaults to always returning code 501, (for servers that are only serving web
      sockets and no other resources via HTTP) *)
  -> ?handshake_timeout:Time.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> ?should_process_request:should_process_request
  (** [should_process_request] allows the user to deny access for a given request, before
      handling any RPCs, or serving web requests from a client *)
  -> ?on_handshake_error:Async.Rpc.Connection.on_handshake_error
  (** default is [`Ignore] *)
  -> 'r

type ('s, 'r, 'l) serve_args =
  where_to_listen:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
  -> ( 's
     , unit
     , ?on_handler_error:
       [ `Raise | `Ignore | `Call of Socket.Address.Inet.t -> exn -> unit ]
     -> ?mode:Conduit_async.server
     -> ?backlog:int
     -> ?max_connections:int
     -> unit
     -> 'r )
       common_args

type 'l tcp_server = (Socket.Address.Inet.t, 'l) Tcp.Server.t Deferred.t
type 'l ws_server = (Socket.Address.Inet.t, 'l) Cohttp_async.Server.t Deferred.t

(** This returns a http handler that can be added into an existing cohttp server *)
val handler : ?description:Info.t -> ('s, 'c, 'c -> raw_http_handler) common_args

(** Serves both HTTP/Websockets and regular RPCs via TCP server. *)
val serve_with_tcp_server
  :  where_to_listen_for_tcp:(Socket.Address.Inet.t, 'l) Tcp.Where_to_listen.t
  -> ?max_message_size:int
  -> ?make_transport:Async.Rpc.Connection.transport_maker
  -> ('s, 'l tcp_server * 'l ws_server, 'l) serve_args

(** Serves HTTP/Websockets only *)
val serve : ('s, 'l ws_server, 'l) serve_args

(** Connect to a Websockets RPC server at the given URI. *)
val client
  :  ?headers:Cohttp.Header.t
  -> ?handshake_timeout:Time_ns.Span.t
  -> ?heartbeat_config:Async.Rpc.Connection.Heartbeat_config.t
  -> Uri.t
  -> Async.Rpc.Connection.t Deferred.Or_error.t
