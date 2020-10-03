open Core
open Async
module Pipe_transport = Async_rpc_kernel.Pipe_transport

module Connection_source = struct
  type 'a t =
    | Web of 'a
    | Plain_tcp
  [@@deriving sexp_of]
end

module Connection_initiated_from = struct
  type t =
    | Websocket_request of Cohttp.Request.t
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

type ('s, 'c, 'r) common_args =
  implementations:'s Rpc.Implementations.t
  -> initial_connection_state:
       ('c
        -> Connection_initiated_from.t
        -> Socket.Address.Inet.t
        -> Rpc.Connection.t
        -> 's)
  -> ?http_handler:('c -> http_handler)
  -> ?handshake_timeout:Time.Span.t
  -> ?heartbeat_config:Rpc.Connection.Heartbeat_config.t
  -> ?should_process_request:should_process_request
  -> ?on_handshake_error:Rpc.Connection.on_handshake_error
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

let default_http_handler _ ~body:_ _ _ = Cohttp_async.Server.respond (`Code 501)

let handler
      ?(description = Info.of_string "HTTP (WS) server")
      ~implementations
      ~initial_connection_state
      ?(http_handler = default_http_handler)
      ?handshake_timeout
      ?heartbeat_config
      ?should_process_request
      ?(on_handshake_error = `Ignore)
      extra_info
  =
  let ws_handler
        client_identity
        ~inet
        ~subprotocol:(_ : string option)
        request
        reader
        writer
    =
    let connection_state =
      initial_connection_state
        client_identity
        (Connection_initiated_from.Websocket_request request)
        inet
    in
    let transport = Pipe_transport.create Pipe_transport.Kind.string reader writer in
    let%bind connection =
      Async_rpc_kernel.Rpc.Connection.create
        ?handshake_timeout:
          (Option.map handshake_timeout ~f:Time_ns.Span.of_span_float_round_nearest)
        ?heartbeat_config
        ~implementations
        ~description
        ~connection_state
        transport
    in
    let%bind () =
      match connection with
      | Ok connection -> Rpc.Connection.close_finished connection
      | Error handshake_error ->
        (match on_handshake_error with
         | `Ignore -> ()
         | `Raise -> raise handshake_error
         | `Call func -> func handshake_error);
        return ()
    in
    Rpc.Transport.close transport
  in
  let should_process_request =
    Option.map
      should_process_request
      ~f:(fun should_process_request inet header ~is_websocket_request ->
        should_process_request
          inet
          (Connection_source.Web (header, `is_websocket_request is_websocket_request)))
  in
  Cohttp_async_websocket.Server.(
    create
      ~opcode:`Binary
      ~non_ws_request:(http_handler extra_info)
      ?should_process_request
      (fun ~inet ~subprotocol request ->
         return (On_connection.create (ws_handler extra_info ~inet ~subprotocol request))))
;;

let serve
      ~where_to_listen
      ~implementations
      ~initial_connection_state
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?should_process_request
      ?on_handshake_error
      ?(on_handler_error = `Ignore)
      ?mode
      ?backlog
      ?max_connections
      ()
  =
  let description =
    let info =
      match mode with
      | None | Some `TCP -> "HTTP (WS) server"
      | Some (`OpenSSL _) | Some (`OpenSSL_with_trust_chain _) -> "HTTPS (WSS) server"
    in
    Info.of_string info
  in
  let handler =
    handler
      ~description
      ~implementations
      ~initial_connection_state
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?should_process_request
      ?on_handshake_error
      ()
  in
  Cohttp_async.Server.create_expert
    ?max_connections
    ?backlog
    ~on_handler_error
    ?mode
    where_to_listen
    handler
;;

let serve_with_tcp_server
      ~where_to_listen_for_tcp
      ?max_message_size
      ?make_transport
      ~where_to_listen
      ~implementations
      ~initial_connection_state
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?should_process_request
      ?on_handshake_error
      ?on_handler_error
      ?mode
      ?backlog
      ?max_connections
      ()
  =
  let ws_server =
    serve
      ()
      ~implementations
      ~initial_connection_state
      ~where_to_listen
      ?http_handler
      ?handshake_timeout
      ?heartbeat_config
      ?should_process_request
      ?on_handshake_error
      ?on_handler_error
      ?mode
      ?backlog
      ?max_connections
  in
  let initial_connection_state addr conn =
    initial_connection_state () Connection_initiated_from.Tcp addr conn
  in
  let auth_opt_to_tcp_auth auth_opt =
    Option.map auth_opt ~f:(fun should_process_request inet ->
      Or_error.is_ok (should_process_request inet Connection_source.Plain_tcp))
  in
  let tcp_server =
    Rpc.Connection.serve
      ()
      ~implementations
      ~initial_connection_state
      ~where_to_listen:where_to_listen_for_tcp
      ?max_connections
      ?backlog
      ?max_message_size
      ?make_transport
      ?handshake_timeout
      ?heartbeat_config
      ?auth:(auth_opt_to_tcp_auth should_process_request)
  in
  tcp_server, ws_server
;;

let connection_create ?handshake_timeout ?heartbeat_config transport =
  Async_rpc_kernel.Rpc.Connection.create
    ~connection_state:(fun _ -> ())
    ?handshake_timeout
    ?heartbeat_config
    transport
  >>| Or_error.of_exn_result
;;

let client ?headers ?handshake_timeout ?heartbeat_config uri =
  let open Deferred.Or_error.Let_syntax in
  let%bind _resp, reader, writer = Cohttp_async_websocket.Client.create ?headers uri in
  let transport = Pipe_transport.create Pipe_transport.Kind.string reader writer in
  connection_create ?handshake_timeout ?heartbeat_config transport
;;
