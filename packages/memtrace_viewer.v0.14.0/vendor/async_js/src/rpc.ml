module Async_js_debug = Debug
open Core_kernel
open Async_kernel
open Js_of_ocaml

module Uri = struct
  include Uri

  let sexp_of_t = Uri_sexp.sexp_of_t
end

module Websocket_connection = struct
  module T = Async_rpc_kernel.Rpc.Connection

  type ('rest, 'implementations) client_t =
    ?uri:Uri.t
    -> ?heartbeat_config:T.Heartbeat_config.t
    -> ?description:Info.t
    -> ?implementations:'implementations T.Client_implementations.t
    -> 'rest

  module Close_code : sig
    type t [@@deriving sexp]

    val to_int : t -> int
    val of_event : _ WebSockets.closeEvent Js.t -> t
    val normal_closure : t
  end = struct
    type close_event_code = int [@@deriving compare, sexp]

    let all_of_close_event_code = []

    type t =
      | Normal_closure
      | Going_away
      | Protocol_error
      | Unsupported_data
      | No_status_Received
      | Abnormal_closure
      | Invalid_frame_payload_data
      | Policy_violation
      | Message_too_big
      | Missing_extension
      | Internal_error
      | Service_restart
      | Try_again_later
      | Bad_gateway
      | TLS_handshake
      | Unknown_close_event_code of close_event_code
      | Invalid_close_event_code of close_event_code
    [@@deriving compare, enumerate, sexp]

    let normal_closure = Normal_closure

    let of_int = function
      | 1000 -> Normal_closure
      | 1001 -> Going_away
      | 1002 -> Protocol_error
      | 1003 -> Unsupported_data
      | 1005 -> No_status_Received
      | 1006 -> Abnormal_closure
      | 1007 -> Invalid_frame_payload_data
      | 1008 -> Policy_violation
      | 1009 -> Message_too_big
      | 1010 -> Missing_extension
      | 1011 -> Internal_error
      | 1012 -> Service_restart
      | 1013 -> Try_again_later
      | 1014 -> Bad_gateway
      | 1015 -> TLS_handshake
      | code ->
        if code >= 3000 && code <= 4999
        then Unknown_close_event_code code
        else Invalid_close_event_code code
    ;;

    let of_event close_event = of_int close_event##.code

    let to_int = function
      | Normal_closure -> 1000
      | Going_away -> 1001
      | Protocol_error -> 1002
      | Unsupported_data -> 1003
      | No_status_Received -> 1005
      | Abnormal_closure -> 1006
      | Invalid_frame_payload_data -> 1007
      | Policy_violation -> 1008
      | Message_too_big -> 1009
      | Missing_extension -> 1010
      | Internal_error -> 1011
      | Service_restart -> 1012
      | Try_again_later -> 1013
      | Bad_gateway -> 1014
      | TLS_handshake -> 1015
      | Unknown_close_event_code code -> code
      | Invalid_close_event_code code -> code
    ;;

    let%test_unit "to_int -> of_int round-trip" =
      List.iter all ~f:(fun t -> assert ([%compare.equal: t] (of_int (to_int t)) t))
    ;;
  end

  let close_websocket (websocket : WebSockets.webSocket Js.t) reason =
    (* [close_withCodeAndReason] does nothing if the connection is already Closed. *)
    let close reason =
      (* The code must be either 1000 (Normal_closure), or between 3000 and 4999
         (Unknown_close_event_code). *)
      websocket##close_withCodeAndReason
        Close_code.(to_int normal_closure)
        (Js.string reason)
    in
    match close reason with
    | () -> ()
    | exception (_ : Exn.t) ->
      (* this can fail if [reason] is too long or contains invalid UTF8 *)
      close "close-reason-was-too-long-or-contains-invalid-utf8"
  ;;

  let connect_websocket url ~from_server ~to_server =
    match new%js WebSockets.webSocket (Js.string (Uri.to_string url)) with
    | exception Js.Error exn ->
      (* e.g. SECURITY_ERR, though note that e.g. connecting to ws:// from a https:// page
         in chrome seems to manifest as successful construction but immediate closure
         (see below).  *)
      return (Or_error.error_string (Js.to_string exn##.message))
    | exception exn -> return (Or_error.of_exn exn)
    | websocket ->
      let connected_ivar = Ivar.create () in
      let cleanup ~reason =
        (* If we haven't connected yet, then connecting just failed. Otherwise it was the
           closure of an existing connection. *)
        Ivar.fill_if_empty connected_ivar (Error reason);
        (* [Pipe.close] and [WebSocket.close] are all idempotent. *)
        Pipe.close from_server;
        Pipe.close_read to_server;
        close_websocket websocket (Error.to_string_hum reason)
      in
      let onclose (close_event : _ WebSockets.closeEvent Js.t) =
        (* The RPC API doesn't give us a good place to put details about _why_ the
           connection closed.

           Note that in Chrome [close_event##.reason] is typically empty and
           [close_event##.code] doesn't actually provide that much insight into why the
           connection failed (it's available in the inspector console, but as far as I can
           tell, not available to us). So it's not like we're losing much anyway. *)
        let reason =
          let what_happened =
            match Ivar.is_full connected_ivar with
            | false -> "connection failed"
            | true -> "closed"
          in
          sprintf
            !"WebSocket %s (%{sexp:Close_code.t})"
            what_happened
            (Close_code.of_event close_event)
        in
        cleanup ~reason:(Error.of_string reason);
        Js._false
      in
      let onmessage (event : _ WebSockets.messageEvent Js.t) =
        let data = Typed_array.Bigstring.of_arrayBuffer event##.data_buffer in
        Pipe.write_without_pushback_if_open from_server data;
        Js._false
      in
      let connected () = Ivar.fill_if_empty connected_ivar (Ok ()) in
      websocket##.binaryType := Js.string "arraybuffer";
      (* if the websocket is already closed, [onclose] won't be re-fired when we add our
         handler. *)
      (match websocket##.readyState with
       | CONNECTING ->
         websocket##.onopen
         := Dom.handler (fun (_ : _ Dom.event Js.t) ->
           connected ();
           Js._false)
       | OPEN -> connected ()
       | CLOSING | CLOSED ->
         (* e.g., Refused to connect to ws: because it violates Content Security Policy *)
         cleanup ~reason:(Error.of_string "WebSocket failed immediately (illegal URI?)"));
      (* Upon an error, [onerror] fires and then [onclose] fires (it's possible for a
         graceful closure to call [onclose] only). Since Async_RPC has no notion of
         graceful closure, we only need to handle [onclose] anyway. Further, the event
         passed to [onerror] contains no extra information about the error.  Note that we
         still listen to [onerror] to prevent the error from leaking to uncontrolled
         context *)
      websocket##.onerror
      := Dom.handler (fun (_ : _ Dom.event Js.t) ->
        Async_js_debug.log_s [%message "websocket encountered unexpected error"];
        Js._false);
      websocket##.onmessage := Dom.handler onmessage;
      websocket##.onclose := Dom.handler onclose;
      let connected_deferred = Ivar.read connected_ivar in
      don't_wait_for
        (match%bind connected_deferred with
         | Error (_ : Error.t) -> return ()
         | Ok () ->
           Pipe.iter_without_pushback to_server ~f:(fun data ->
             match (websocket##.readyState : WebSockets.readyState) with
             | CONNECTING ->
               raise_s [%message "BUG: onopen called but readyState is CONNECTING"]
             | CLOSING | CLOSED -> ()
             | OPEN ->
               let buffer = Typed_array.Bigstring.to_arrayBuffer data in
               (try websocket##send_buffer buffer with
                | exn ->
                  Error.raise (Error.tag ~tag:"websocket##send_buffer" (Error.of_exn exn)))));
      let cleanup_when_a_pipe_is_closed =
        let%map () =
          Deferred.any_unit [ Pipe.closed to_server; Pipe.closed from_server ]
        in
        cleanup ~reason:(Error.of_string "RPC connection closed by client")
      in
      don't_wait_for cleanup_when_a_pipe_is_closed;
      connected_deferred
  ;;

  let default_uri () =
    let scheme = if String.equal Url.Current.protocol "https:" then "wss" else "ws" in
    let port =
      match Url.Current.port with
      | Some port -> port
      | None ->
        if String.equal Url.Current.protocol "https:"
        then Url.default_https_port
        else Url.default_http_port
    in
    let host = Url.Current.host in
    Uri.make ~scheme ~host ~port ()
  ;;

  let client ?(uri = default_uri ()) ?heartbeat_config ?description ?implementations () =
    let description =
      Info.create_s
        [%message "websocket" (description : Info.t sexp_option) (uri : Uri.t)]
    in
    let make_connection transport =
      let create { T.Client_implementations.connection_state; implementations } =
        T.create
          transport
          ?heartbeat_config
          ~description
          ~implementations
          ~connection_state
      in
      match implementations with
      | None -> create (T.Client_implementations.null ())
      | Some implementations -> create implementations
    in
    let from_server_r, from_server_w = Pipe.create () in
    let to_server_r, to_server_w = Pipe.create () in
    match%bind
      connect_websocket uri ~from_server:from_server_w ~to_server:to_server_r
    with
    | Error _ as error -> return error
    | Ok () ->
      let transport =
        Async_rpc_kernel.Pipe_transport.create
          Async_rpc_kernel.Pipe_transport.Kind.bigstring
          from_server_r
          to_server_w
      in
      (match%bind make_connection transport with
       | Ok connection -> return (Ok connection)
       | Error exn ->
         let%bind () = Async_rpc_kernel.Rpc.Transport.close transport in
         return (Or_error.of_exn exn))
  ;;

  let client_exn ?uri ?heartbeat_config ?description ?implementations () =
    client ?uri ?heartbeat_config ?description ?implementations () >>| Or_error.ok_exn
  ;;
end

include Async_rpc_kernel.Rpc

module Connection = struct
  include Connection
  include Websocket_connection
end
