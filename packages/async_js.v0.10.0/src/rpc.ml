open Core_kernel
open Async_kernel
open Js_of_ocaml
open Ocaml_uri
open Import

module Pipe_transport  = Rpc_kernel.Pipe_transport
module Any             = Rpc_kernel.Rpc.Any
module Description     = Rpc_kernel.Rpc.Description
module Implementation  = Rpc_kernel.Rpc.Implementation
module Implementations = Rpc_kernel.Rpc.Implementations
module One_way         = Rpc_kernel.Rpc.One_way
module Pipe_rpc        = Rpc_kernel.Rpc.Pipe_rpc
module Rpc             = Rpc_kernel.Rpc.Rpc
module State_rpc       = Rpc_kernel.Rpc.State_rpc

module Pipe_close_reason = Rpc_kernel.Rpc.Pipe_close_reason

module Connection = struct
  include Rpc_kernel.Rpc.Connection

  type ('rest, 'implementations) client_t
    =  ?uri              : Uri.t
    -> ?heartbeat_config : Heartbeat_config.t
    -> ?description      : Info.t
    -> ?implementations  : 'implementations Client_implementations.t
    -> 'rest

  (* https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent *)
  let string_of_error_code = function
    | 1000 -> "CLOSE_NORMAL"
    | 1001 -> "CLOSE_GOING_AWAY"
    | 1002 -> "CLOSE_PROTOCOL_ERROR"
    | 1003 -> "CLOSE_UNSUPPORTED"
    | 1005 -> "CLOSE_NO_STATUS"
    | 1006 -> "CLOSE_ABNORMAL"
    | 1007 -> "Unsupported Data"
    | 1008 -> "Policy Violation"
    | 1009 -> "CLOSE_TOO_LARGE"
    | 1010 -> "Missing Extension"
    | 1011 -> "Internal Error"
    | 1012 -> "Service Restart"
    | 1013 -> "Try Again Later"
    | 1015 -> "TLS Handshake"
    | code -> sprintf "Unknown CloseEvent code: %d" code


  let pipe_of_websocket url =
    let url = Uri.to_string url in
    let ws = new%js WebSockets.webSocket (Js.string url) in
    let reader_r, reader_w = Pipe.create () in
    let writer_r, writer_w = Pipe.create () in
    let fatal_error = ref false in
    let close () =
      Pipe.close writer_w;
      Pipe.close reader_w;
      Pipe.close_read reader_r;
      Pipe.close_read writer_r;
      fatal_error := true
    in
    let monitor = Monitor.current () in
    let onclose _this close_event =
      if not (Js.to_bool (close_event##.wasClean) && close_event##.code = 1000)
      then
        begin
          let reason = Js.to_string close_event##.reason in
          let reason = if reason = "" then "unknown reason" else reason in
          let cleanly =
            if Js.to_bool close_event##.wasClean
            then " cleanly"
            else ""
          in
          Monitor.send_exn monitor
            (Error.to_exn (
               Error.createf
                 "Connection closed%s: %s (%s)" cleanly reason (string_of_error_code close_event##.code)
             ))
        end;
      close ();
      Js._false;
    in
    let onerror _this _event =
      Monitor.send_exn monitor (Error.to_exn (Error.createf "Connection close with error"));
      close ();
      Js._false
    in
    let onopen ws _event =
      ws##.onmessage := Dom.full_handler begin
        fun _ (event : _ WebSockets.messageEvent Js.t) ->
          if not !fatal_error then begin
            let data = Typed_array.Bigstring.of_arrayBuffer event##.data_buffer in
            Pipe.write_without_pushback reader_w data;
            Js._true
          end else Js._false
      end;
      don't_wait_for begin
        Pipe.iter_without_pushback writer_r ~f:(fun data ->
          match (ws##.readyState : WebSockets.readyState) with
          | CLOSING | CLOSED -> ()
          | CONNECTING | OPEN when !fatal_error -> ()
          | CONNECTING | OPEN ->
            let buffer = Typed_array.Bigstring.to_arrayBuffer data in
            ws##send_buffer buffer
        )
      end;
      Js._true
    in
    ws##.binaryType := Js.string "arraybuffer";
    ws##.onopen  := Dom.full_handler onopen;
    ws##.onerror := Dom.full_handler onerror;
    ws##.onclose := Dom.full_handler onclose;
    let close_because pipe reason =
      Pipe.closed pipe >>| fun () ->
      match ws##.readyState with
      | CLOSING | CLOSED -> ()
      | CONNECTING | OPEN -> ws##close_withCodeAndReason (1000) (Js.string reason)
    in
    don't_wait_for (close_because writer_w "Client closed writer pipe");
    don't_wait_for (close_because reader_r "Client closed reader pipe");
    reader_r, writer_w

  let client
        ?uri
        ?heartbeat_config
        ?description
        ?implementations
        ()
    =
    let uri =
      match uri with
      | Some uri -> uri
      | None ->
        let scheme =
          if Url.Current.protocol = "https:"
          then "wss"
          else "ws"
        in
        let port =
          match Url.Current.port with
          | Some port -> port
          | None ->
            if Url.Current.protocol = "https:"
            then Url.default_https_port
            else Url.default_http_port
        in
        let host = Url.Current.host in
        Uri.make ~scheme ~host ~port ()
    in
    let pipe_reader,pipe_writer = pipe_of_websocket uri in
    let description = match description with
      | None -> Info.create "Client connected via WS" uri Uri.sexp_of_t
      | Some desc -> Info.tag_arg desc "via WS" uri Uri.sexp_of_t
    in
    let transport = Pipe_transport.create Pipe_transport.Kind.bigstring pipe_reader pipe_writer in
    begin
      match implementations with
      | None ->
        let { Client_implementations. connection_state; implementations } =
          Client_implementations.null ()
        in
        create
          transport
          ?heartbeat_config
          ~implementations
          ~description
          ~connection_state
      | Some { Client_implementations. connection_state; implementations } ->
        create
          transport
          ?heartbeat_config
          ~implementations
          ~description
          ~connection_state
    end
    >>= function
    | Ok connection ->
      return (Ok connection)
    | Error exn ->
      Async_rpc_kernel.Rpc.Transport.close transport
      >>| fun () ->
      Error (Error.of_exn exn)

  let client_exn
        ?uri
        ?heartbeat_config
        ?description
        ?implementations
        ()
    =
    client
      ?uri
      ?heartbeat_config
      ?description
      ?implementations
      ()
    >>| Or_error.ok_exn
end
