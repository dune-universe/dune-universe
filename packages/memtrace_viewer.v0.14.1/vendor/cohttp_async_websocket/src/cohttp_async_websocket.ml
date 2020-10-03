open! Core
open! Async
open! Import
module Header = Header

let websocket_accept_header_name = "Sec-Websocket-Accept"

module Server = struct
  module On_connection = struct
    type t =
      { set_response_headers : Header.t
      ; should_overwrite_sec_accept_header : bool
      ; handle_connection :
          string Pipe.Reader.t -> string Pipe.Writer.t -> unit Deferred.t
      }

    let create
          ?(set_response_headers = Header.init ())
          ?(should_overwrite_sec_accept_header = true)
          handle_connection
      =
      { set_response_headers; should_overwrite_sec_accept_header; handle_connection }
    ;;
  end

  type websocket_handler =
    inet:Socket.Address.Inet.t
    -> subprotocol:string option
    -> Cohttp.Request.t
    -> On_connection.t Deferred.t

  (* {v
       [1] https://tools.ietf.org/html/rfc6455#section-1.3
       1.3.  Opening Handshake

          _This section is non-normative._

          The opening handshake is intended to be compatible with HTTP-based
          server-side software and intermediaries, so that a single port can be
          used by both HTTP clients talking to that server and WebSocket
          clients talking to that server.  To this end, the WebSocket client's
          handshake is an HTTP Upgrade request:

               GET /chat HTTP/1.1
               Host: server.example.com
               Upgrade: websocket
               Connection: Upgrade
               Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==
               Origin: http://example.com
               Sec-WebSocket-Protocol: chat, superchat
               Sec-WebSocket-Version: 13

          In compliance with [RFC2616], header fields in the handshake may be
          sent by the client in any order, so the order in which different
          header fields are received is not significant.

          The "Request-URI" of the GET method [RFC2616] is used to identify the
          endpoint of the WebSocket connection, both to allow multiple domains
          to be served from one IP address and to allow multiple WebSocket
          endpoints to be served by a single server.

          The client includes the hostname in the |Host| header field of its
          handshake as per [RFC2616], so that both the client and the server
          can verify that they agree on which host is in use.

          Additional header fields are used to select options in the WebSocket
          Protocol.  Typical options available in this version are the
          subprotocol selector (|Sec-WebSocket-Protocol|), list of extensions
          support by the client (|Sec-WebSocket-Extensions|), |Origin| header
          field, etc.  The |Sec-WebSocket-Protocol| request-header field can be
          used to indicate what subprotocols (application-level protocols
          layered over the WebSocket Protocol) are acceptable to the client.
          The server selects one or none of the acceptable protocols and echoes
          that value in its handshake to indicate that it has selected that
          protocol.

               Sec-WebSocket-Protocol: chat

          The |Origin| header field [RFC6454] is used to protect against
          unauthorized cross-origin use of a WebSocket server by scripts using
          the WebSocket API in a web browser.  The server is informed of the
          script origin generating the WebSocket connection request.  If the
          server does not wish to accept connections from this origin, it can
          choose to reject the connection by sending an appropriate HTTP error
          code.  This header field is sent by browser clients; for non-browser
          clients, this header field may be sent if it makes sense in the
          context of those clients.

          Finally, the server has to prove to the client that it received the
          client's WebSocket handshake, so that the server doesn't accept
          connections that are not WebSocket connections.  This prevents an
          attacker from tricking a WebSocket server by sending it carefully
          crafted packets using XMLHttpRequest [XMLHttpRequest] or a form
          submission.

          To prove that the handshake was received, the server has to take two
          pieces of information and combine them to form a response.  The first
          piece of information comes from the |Sec-WebSocket-Key| header field
          in the client handshake:

               Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==

          For this header field, the server has to take the value (as present
          in the header field, e.g., the base64-encoded [RFC4648] version minus
          any leading and trailing whitespace) and concatenate this with the
          Globally Unique Identifier (GUID, [RFC4122]) "258EAFA5-E914-47DA-
          95CA-C5AB0DC85B11" in string form, which is unlikely to be used by
          network endpoints that do not understand the WebSocket Protocol.  A
          SHA-1 hash (160 bits) [FIPS.180-3], base64-encoded (see Section 4 of
          [RFC4648]), of this concatenation is then returned in the server's
          handshake.

          Concretely, if as in the example above, the |Sec-WebSocket-Key|
          header field had the value "dGhlIHNhbXBsZSBub25jZQ==", the server
          would concatenate the string "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
          to form the string "dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-
          C5AB0DC85B11".  The server would then take the SHA-1 hash of this,
          giving the value 0xb3 0x7a 0x4f 0x2c 0xc0 0x62 0x4f 0x16 0x90 0xf6
          0x46 0x06 0xcf 0x38 0x59 0x45 0xb2 0xbe 0xc4 0xea.  This value is
          then base64-encoded (see Section 4 of [RFC4648]), to give the value
          "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=".  This value would then be echoed in
          the |Sec-WebSocket-Accept| header field.

          The handshake from the server is much simpler than the client
          handshake.  The first line is an HTTP Status-Line, with the status
          code 101:

               HTTP/1.1 101 Switching Protocols

          Any status code other than 101 indicates that the WebSocket handshake
          has not completed and that the semantics of HTTP still apply.  The
          headers follow the status code.

          The |Connection| and |Upgrade| header fields complete the HTTP
          Upgrade.  The |Sec-WebSocket-Accept| header field indicates whether
          the server is willing to accept the connection.  If present, this
          header field must include a hash of the client's nonce sent in
          |Sec-WebSocket-Key| along with a predefined GUID.  Any other value
          must not be interpreted as an acceptance of the connection by the
          server.

               HTTP/1.1 101 Switching Protocols
               Upgrade: websocket
               Connection: Upgrade
               Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=

          These fields are checked by the WebSocket client for scripted pages.
          If the |Sec-WebSocket-Accept| value does not match the expected
          value, if the header field is missing, or if the HTTP status code is
          not 101, the connection will not be established, and WebSocket frames
          will not be sent.

          Option fields can also be included.  In this version of the protocol,
          the main option field is |Sec-WebSocket-Protocol|, which indicates
          the subprotocol that the server has selected.  WebSocket clients
          verify that the server included one of the values that was specified
          in the WebSocket client's handshake.  A server that speaks multiple
          subprotocols has to make sure it selects one based on the client's
          handshake and specifies it in its handshake.

               Sec-WebSocket-Protocol: chat

          The server can also set cookie-related option fields to _set_
          cookies, as described in [RFC6265].
     v}

     Client Request:
     GET <resourcename> HTTP/1.1
     Host: server.example.com *
     Upgrade: websocket
     Connection: Upgrade
     Sec-WebSocket-Key: <key>
     Origin: <origin>
     Sec-WebSocket-Protocol: <protocol>
     Sec-WebSocket-Version: <version>

     Server Response:
     HTTP/1.1 101 Switching Protocols
     Upgrade: websocket
     Connection: Upgrade
     Sec-WebSocket-Accept: <key>
  *)
  let websocket_handshake_headers
        ~initial_headers:header
        ~sec_websocket_key
        ~should_overwrite_sec_accept_header
        ~subprotocol
    =
    let maybe_overwrite_sec_accept_header header =
      if should_overwrite_sec_accept_header
      then
        Header.replace
          header
          websocket_accept_header_name
          (Websocket.sec_websocket_accept_header_value ~sec_websocket_key)
      else header
    in
    let header = Header.add_unless_exists header "Upgrade" "websocket" in
    let header = Header.add_unless_exists header "Connection" "upgrade" in
    let header = Header.add_transfer_encoding header Unknown in
    let header = maybe_overwrite_sec_accept_header header in
    Option.value_map subprotocol ~default:header ~f:(fun subprotocol ->
      Header.add_websocket_subprotocol header ~subprotocol)
  ;;

  module Expect_test_config = Core.Expect_test_config

  (* {v https://tools.ietf.org/html/rfc6455#section-10.2
            10.2.  Origin Considerations

              Servers that are not intended to process input from any web page but
              only for certain sites SHOULD verify the |Origin| field is an origin
              they expect.  If the origin indicated is unacceptable to the server,
              then it SHOULD respond to the WebSocket handshake with a reply
              containing HTTP 403 Forbidden status code.

              The |Origin| header field protects from the attack cases when the
              untrusted party is typically the author of a JavaScript application
              that is executing in the context of the trusted client.  The client
              itself can contact the server and, via the mechanism of the |Origin|
              header field, determine whether to extend those communication
              privileges to the JavaScript application.  The intent is not to
              prevent non-browsers from establishing connections but rather to
              ensure that trusted browsers under the control of potentially
              malicious JavaScript cannot fake a WebSocket handshake.
         v}
  *)
  let detect_request_type_and_authorize ~auth ~inet headers =
    let open Result.Let_syntax in
    let maybe_websocket_key = Header.get headers "sec-websocket-key" in
    let%map () =
      auth inet headers ~is_websocket_request:(Option.is_some maybe_websocket_key)
    in
    match maybe_websocket_key with
    | None -> `Not_a_websocket_request
    | Some sec_websocket_key -> `Websocket_request (`Sec_websocket_key sec_websocket_key)
  ;;

  let default_auth (_ : Socket.Address.Inet.t) header ~is_websocket_request =
    if is_websocket_request then Header.origin_and_host_match header else Ok ()
  ;;

  let%test_module _ =
    (module struct
      let irrelevant_inet = Socket.Address.Inet.create_bind_any ~port:0

      let check ~auth headers =
        print_s
          [%sexp
            (detect_request_type_and_authorize ~inet:irrelevant_inet ~auth headers
             : [ `Not_a_websocket_request
               | `Websocket_request of [ `Sec_websocket_key of string ]
               ]
                 Or_error.t)]
      ;;

      let%expect_test "Only perform websocket validation if the request is for a \
                       websocket upgrade"
        =
        let check = check ~auth:default_auth in
        check (Header.of_list [ "host", "valid-host"; "origin", "https://bogus" ]);
        [%expect {| (Ok Not_a_websocket_request) |}];
        check (Header.of_list [ "sec-websocket-key", "not-important" ]);
        [%expect
          {|
      (Error ("Missing one of origin or host header" (origin ()) (host ()))) |}];
        check
          (Header.of_list
             [ "origin", "http://h"; "host", "h"; "sec-websocket-key", "not-important" ]);
        [%expect {|
      (Ok (Websocket_request (Sec_websocket_key not-important))) |}]
      ;;

      let%expect_test "detect_request_type_and_authorize provides correct \
                       [is_websocket_request] and faithfully returns the result of the \
                       auth function"
        =
        let auth response address headers ~is_websocket_request =
          print_s
            [%sexp
              { address : Socket.Address.Inet.t
              ; is_websocket_request : bool
              ; headers : Header.t
              }];
          response
        in
        let check response headers = check ~auth:(auth response) headers in
        let non_websocket_headers =
          Header.of_list [ "host", "valid-host"; "origin", "https://bogus" ]
        in
        let websocket_headers = Header.of_list [ "sec-websocket-key", "not-important" ] in
        let fail = error_s [%message "fail"] in
        check (Ok ()) non_websocket_headers;
        [%expect
          {|
          ((address 0.0.0.0:PORT) (is_websocket_request false)
           (headers ((host valid-host) (origin https://bogus))))
          (Ok Not_a_websocket_request) |}];
        check fail non_websocket_headers;
        [%expect
          {|
      ((address 0.0.0.0:PORT) (is_websocket_request false)
       (headers ((host valid-host) (origin https://bogus))))
      (Error fail) |}];
        check (Ok ()) websocket_headers;
        [%expect
          {|
      ((address 0.0.0.0:PORT) (is_websocket_request true)
       (headers ((sec-websocket-key not-important))))
      (Ok (Websocket_request (Sec_websocket_key not-important))) |}];
        check fail websocket_headers;
        [%expect
          {|
      ((address 0.0.0.0:PORT) (is_websocket_request true)
       (headers ((sec-websocket-key not-important))))
      (Error fail) |}]
      ;;
    end)
  ;;

  let forbidden request e =
    Log.Global.error_s
      [%message
        "Failed to validate apparent websocket request"
          ~_:(e : Error.t)
          ~_:(request : Request.t)];
    return (`Response (Response.make () ~status:`Forbidden, Body.empty))
  ;;

  let create
        ~non_ws_request
        ?(opcode = `Text)
        ?(should_process_request = default_auth)
        ?(websocket_subprotocol_selection = Fn.const (`Subprotocol None))
        (f : websocket_handler)
        ~body
        inet
        request
    =
    let headers = request.Request.headers in
    match
      detect_request_type_and_authorize ~auth:should_process_request ~inet headers
    with
    | Error e -> forbidden request e
    | Ok (`Websocket_request (`Sec_websocket_key sec_websocket_key)) ->
      let (`Subprotocol subprotocol) = websocket_subprotocol_selection request in
      let%bind { set_response_headers
               ; should_overwrite_sec_accept_header
               ; handle_connection
               }
        =
        f ~inet ~subprotocol request
      in
      let headers =
        websocket_handshake_headers
          ~initial_headers:set_response_headers
          ~sec_websocket_key
          ~should_overwrite_sec_accept_header
          ~subprotocol
      in
      let io_handler reader writer =
        let websocket = Websocket.create ~opcode ~role:Server reader writer in
        let reader, writer = Websocket.pipes websocket in
        Deferred.all_unit
          [ handle_connection reader writer
          ; Pipe.closed reader
          ; Pipe.closed writer
          ; Deferred.ignore_m (Websocket.close_finished websocket)
          ]
      in
      let response =
        Response.make
          ()
          ~encoding:(Header.get_transfer_encoding headers)
          ~status:(`Code 101)
          ~headers
      in
      return (`Expert (response, io_handler))
    | Ok `Not_a_websocket_request ->
      let%map r = non_ws_request ~body inet request in
      `Response r
  ;;
end

module Client = struct
  let random_key () =
    let chars =
      String.init 16 ~f:(fun _ ->
        Char.of_int_exn (Random.int (Char.to_int Char.max_value)))
    in
    Base64.encode_exn chars
  ;;

  let websocket_header ?(headers = Header.init ()) hnp ~scheme =
    let hnp_str = Host_and_port.to_string hnp in
    let header =
      Header.add_list
        headers
        [ "Upgrade", "websocket"
        ; "Connection", "Upgrade"
        ; "Sec-Websocket-Key", random_key ()
        ; "Sec-Websocket-Version", "13"
        ]
    in
    let header = Header.add_unless_exists header "Host" hnp_str in
    Header.add_unless_exists header "Origin" (scheme ^ "://" ^ hnp_str)
  ;;

  let websocket_request ?headers host_and_port uri =
    let insecure_websocket_scheme = "ws" in
    Request.make
      ~encoding:Chunked
      ~headers:
        (websocket_header
           ?headers
           host_and_port
           ~scheme:(Uri.scheme uri |> Option.value ~default:insecure_websocket_scheme))
      ~meth:`GET
      ~version:`HTTP_1_1
      uri
  ;;

  module Request_ = Cohttp.Request.Make (Cohttp_async.Io)
  module Response_ = Cohttp.Response.Make (Cohttp_async.Io)

  let read_websocket_response (request : Request.t) reader =
    match%map Response_.read reader with
    | (`Eof | `Invalid _) as response ->
      error_s
        [%message
          "Bad response to websocket request" (response : [ `Eof | `Invalid of string ])]
    | `Ok response ->
      (match response.status with
       | `Switching_protocols ->
         let websocket_key = "Sec-Websocket-Key" in
         (match Header.get request.headers websocket_key with
          | None ->
            (* This should never happen, the header must be provided, see
               https://tools.ietf.org/html/rfc6455#section-1.3 *)
            error_s [%message "Request missing required header" ~header:websocket_key]
          | Some sec_websocket_key ->
            (* From https://tools.ietf.org/html/rfc6455#section-4.1:

               4.  If the response lacks a |Sec-WebSocket-Accept| header field or
               the |Sec-WebSocket-Accept| contains a value other than the
               base64-encoded SHA-1 of the concatenation of the |Sec-WebSocket-
               Key| (as a string, not base64-decoded) with the string "258EAFA5-
               E914-47DA-95CA-C5AB0DC85B11" but ignoring any leading and
               trailing whitespace, the client MUST _Fail the WebSocket
               Connection_.
            *)
            let expected_sec_websocket_accept =
              Websocket.sec_websocket_accept_header_value ~sec_websocket_key
            in
            (match Header.get response.headers websocket_accept_header_name with
             | Some sec_websocket_accept ->
               if String.equal sec_websocket_accept expected_sec_websocket_accept
               then Ok response
               else
                 error_s
                   [%message
                     "Bad value for header"
                       ~header:websocket_accept_header_name
                       ~value:sec_websocket_accept
                       ~expected:expected_sec_websocket_accept]
             | None ->
               error_s
                 [%message
                   "Missing header"
                     ~header:websocket_accept_header_name
                     expected_sec_websocket_accept]))
       | status ->
         error_s
           [%message
             "Response status code not supported, expected a 101: switching protocols"
               (status : Code.status_code)
               (response.headers : Header.t)
               ~code:(Code.code_of_status status : int)])
  ;;

  let wrap_in_ssl ?hostname_for_ssl reader writer =
    let app_to_ssl_r, app_to_ssl_w = Pipe.create () in
    let ssl_to_app_r, ssl_to_app_w = Pipe.create () in
    let%bind connection =
      let verify_modes =
        if am_running_inline_test then Some [] else None
      in
      Async_ssl.Ssl.client
        ~app_to_ssl:app_to_ssl_r
        ~ssl_to_app:ssl_to_app_w
        ~net_to_ssl:(Reader.pipe reader)
        ~ssl_to_net:(Writer.pipe writer)
        ?verify_modes
        ?hostname:hostname_for_ssl
        ()
      >>| Or_error.ok_exn
    in
    let%bind app_to_ssl, `Closed_and_flushed_downstream _ =
      Writer.of_pipe (Info.of_string "app_to_ssl") app_to_ssl_w
    in
    (* When the pipe (app_to_ssl) is closed, there will be a short period of time when
       the [writer] will still be open. Any message sent to the writer during that time
       will be lost. *)
    Writer.set_raise_when_consumer_leaves app_to_ssl false;
    let%map ssl_to_app = Reader.of_pipe (Info.of_string "ssl_to_app") ssl_to_app_r in
    let close () =
      Async_ssl.Ssl.Connection.close connection;
      let%bind () = Reader.close ssl_to_app in
      Deferred.ignore_m
        (Async_ssl.Ssl.Connection.closed connection : unit Or_error.t Deferred.t)
    in
    close, ssl_to_app, app_to_ssl
  ;;

  let host_and_port_of_uri uri =
    match Uri.host uri with
    | None -> Or_error.error_s [%message "No host given in URI" (uri : Uri_sexp.t)]
    | Some host ->
      (match Uri.port uri with
       | Some port -> Ok (Host_and_port.create ~host ~port)
       | None ->
         (match Uri.scheme uri with
          | Some "ws" -> Ok (Host_and_port.create ~host ~port:80)
          | Some "wss" -> Ok (Host_and_port.create ~host ~port:443)
          | Some scheme ->
            Or_error.error_s
              [%message
                "No port given in URI and using an unknown scheme, couldn't determine port"
                  (scheme : string)
                  (uri : Uri_sexp.t)]
          | None ->
            Or_error.error_s
              [%message
                "Neither port nor scheme given in URI, couldn't determine port"
                  (uri : Uri_sexp.t)]))
  ;;

  let uri_is_ssl uri =
    match Uri.scheme uri with
    | Some "wss" -> true
    | _ -> false
  ;;

  let create ?force_ssl_overriding_SNI_hostname ?headers uri =
    match host_and_port_of_uri uri with
    | Error _ as error -> return error
    | Ok host_and_port ->
      let shutdown = Ivar.create () in
      (match%bind
         Monitor.try_with
           (fun () -> Tcp.connect (Tcp.Where_to_connect.of_host_and_port host_and_port))
           ~rest:
             (`Call
                (fun exn ->
                   Log.Global.sexp
                     [%message "Connection closed. Closing websocket client." (exn : exn)];
                   Ivar.fill_if_empty shutdown ()))
       with
       | Error exn -> return (Or_error.of_exn exn)
       | Ok (_, reader, writer) ->
         let%bind close_tcp_connection, reader, writer =
           match force_ssl_overriding_SNI_hostname with
           | Some hostname_for_ssl -> wrap_in_ssl ~hostname_for_ssl reader writer
           | None ->
             (match uri_is_ssl uri with
              | true ->
                wrap_in_ssl
                  ?hostname_for_ssl:force_ssl_overriding_SNI_hostname
                  reader
                  writer
              | false ->
                let close () = Writer.close writer in
                return (close, reader, writer))
         in
         let request = websocket_request ?headers host_and_port uri in
         let%bind () = Request_.write_header request writer in
         (match%bind read_websocket_response request reader with
          | Error _ as error ->
            let%bind () = close_tcp_connection () in
            Ivar.fill_if_empty shutdown ();
            return error
          | Ok response ->
            let open Deferred.Let_syntax in
            let ws = Websocket.create ~role:Client reader writer in
            let reader, writer = Websocket.pipes ws in
            don't_wait_for
              (let%bind () =
                 Deferred.any [ Ivar.read shutdown; Pipe.closed writer; Pipe.closed reader ]
               in
               Pipe.close writer;
               let%bind () = Pipe.closed writer in
               let%bind () = close_tcp_connection () in
               Pipe.close_read reader;
               let%bind reason, msg, info = Websocket.close_finished ws in
               Log.Global.sexp
                 [%message
                   "Websocket closed"
                     (reason : Websocket.Connection_close_reason.t)
                     (msg : string)
                     (info : (Info.t option[@sexp.omit_nil]))];
               return ());
            return (Ok (response, reader, writer))))
  ;;

  let with_websocket_client ?headers uri ~f =
    match%bind create ?headers uri with
    | Error _ as err -> return err
    | Ok (response, reader, writer) ->
      let%bind result = f response reader writer in
      Pipe.close writer;
      return (Ok result)
  ;;
end
