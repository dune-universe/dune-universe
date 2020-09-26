open Core
open Async
open Cohttp_jane

let implementation_send_string =
  Rpc.Rpc.implement' Async_js_test_lib.Rpcs.send_string (fun (_ : Rpc.Connection.t) str ->
    Log.Global.debug_s [%message "Got request for send_string: " ~_:str];
    str)
;;

let implementation_close_connection =
  Rpc.Rpc.implement' Async_js_test_lib.Rpcs.close_connection (fun conn () ->
    Log.Global.debug_s [%message "Got request for close_connection"];
    don't_wait_for
      (let%bind () = Clock.after Time.Span.second in
       Rpc.Connection.close
         ~reason:(Info.of_string "got request to close from client")
         conn))
;;

let start_http_server () =
  let reader, writer = Pipe.create () in
  let implementations = [ implementation_send_string; implementation_close_connection ] in
  let implementations =
    Rpc.Implementations.create_exn ~implementations ~on_unknown_rpc:`Raise
  in
  let%bind server =
    let on_handler_error inet err =
      Log.Global.error_s
        [%message "Error encountered" (inet : Socket.Address.Inet.t) (err : Exn.t)]
    in
    let http_handler =
      Cohttp_static_handler.Single_page_handler.(
        embedded_js_handler
          default
          ~css:[]
          ~scripts:[ Embedded_files.main_dot_bc_dot_js ]
          ~on_unknown_url:`Not_found)
    in
    Rpc_websocket_jane.Rpc.serve
      ()
      ~implementations
      ~initial_connection_state:(fun () _ _ conn ->
        Pipe.write_without_pushback writer conn;
        conn)
      ~auth_scheme:Authorizer.Authentication_scheme.none
      ~http_handler:(fun () -> http_handler)
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~on_handler_error:(`Call on_handler_error)
  in
  return (server, reader)
;;

let run_test f =
  let () = Log.Global.set_output [] in
  let%bind web_server, connection_pipe = start_http_server () in
  let web_server = Or_error.ok_exn web_server in
  let web_port = Cohttp_async.Server.listening_on web_server in
  Web_testing.Test_client.with_client (fun client -> f web_port connection_pipe client)
;;

(* Cleanup error from non-deterministic bits *)
let rec cleanup =
  let cleanup_name = function
    | "port" -> true
    | (_ : string) -> false
  in
  let open Sexp in
  function
  | Atom str as x ->
    (match Sexp.of_string str with
     | exception _ -> x
     | (Atom _ | List [ _ ]) as x -> x
     | sexp -> cleanup sexp)
  | List l ->
    List
      (List.map l ~f:(function
         | List [ Atom name; _ ] when cleanup_name name ->
           List [ Atom name; Atom (String.uppercase name) ]
         | x -> cleanup x))
;;

let dispatch_and_print ~client s =
  let code =
    let uri = Uri.of_string s in
    Async_js_test_lib.Callback_function.to_javascript_invocation (Open_rpc_and_wait uri)
  in
  let%map response = Web_testing.Test_client.dispatch_js client code in
  let response_sexp =
    match response with
    | Error e -> Error.sexp_of_t e
    | Ok remote_object ->
      Option.value_map ~default:(Sexp.List []) remote_object.value ~f:(fun json ->
        let s =
          match json with
          | `String s -> s
          | _ -> Yojson.Safe.to_string json
        in
        match Sexp.of_string s with
        | exception _ -> Sexp.Atom s
        | sexp -> sexp)
  in
  printf !"%{Sexp#hum}\n%!" (cleanup response_sexp)
;;

let wait_for_the_page_to_be_loaded client =
  match%map
    let the_page_should_load_in_much_less_time_than_this_even_on_hydra =
      Time.Span.of_sec 30.
    in
    Clock.with_timeout
      the_page_should_load_in_much_less_time_than_this_even_on_hydra
      (Web_testing.Test_client.until_text client ~text:"Ready" `Exists)
  with
  | `Timeout -> print_endline "Timeout"
  | `Result () -> print_endline "Loaded"
;;

let print_when_connection_established_exn conn ~f =
  don't_wait_for
    (conn
     >>= function
     | `Eof -> raise_s [%message "Unexpected EOF" [%here]]
     | `Ok conn ->
       print_endline "New connection";
       f conn)
;;

let%expect_test _ =
  let pipe_must_be_empty pipe = [%test_result: _ option] ~expect:None (Pipe.peek pipe) in
  let read_new pipe =
    pipe_must_be_empty pipe;
    Pipe.read pipe
  in
  run_test (fun web_port connection_pipe client ->
    let%bind () =
      Web_testing.Test_client.navigate client (sprintf "http://localhost:%i/" web_port)
    in
    let dispatch_and_print = dispatch_and_print ~client in
    let%bind () = wait_for_the_page_to_be_loaded client in
    [%expect {| Loaded |}];
    (* synchronous failure (attempted use of port 20) *)
    let%bind () = dispatch_and_print "ws://localhost:20/" in
    [%expect {|
"Failed to construct 'WebSocket': The port 20 is not allowed." |}];
    (* synchronous failure (invalid url) *)
    let%bind () = dispatch_and_print "ws://in valid/" in
    [%expect {|
          "WebSocket connection failed (Abnormal_closure)" |}];
    (* synchronous failure (invalid url) *)
    let%bind () = dispatch_and_print (sprintf "ws://localhost:%d/\000" web_port) in
    let%bind () =
      let output = [%expect.output] in
      String.substr_replace_all output ~pattern:(Int.to_string web_port) ~with_:"PORT"
      |> print_endline;
      return ()
    in
    [%expect
      {|
          "Failed to construct 'WebSocket': The URL 'ws://localhost:PORT/%00' is invalid." |}];
    (* immediate failure *)
    let%bind () = dispatch_and_print (sprintf "wss://localhost:%d/" web_port) in
    [%expect {| "WebSocket connection failed (Abnormal_closure)" |}];
    (* immediate failure *)
    let%bind () =
      dispatch_and_print (sprintf "ws://shouldnt-resolve.fakedomain.com/")
    in
    [%expect {| "WebSocket connection failed (Abnormal_closure)" |}];
    (* successful connection, close after handshake *)
    let conn = read_new connection_pipe in
    print_when_connection_established_exn conn ~f:Rpc.Connection.close;
    let%bind () = dispatch_and_print (sprintf "ws://localhost:%d/" web_port) in
    [%expect
      {|
          New connection
          ((rpc_error (Connection_closed ("RPC transport stopped")))
           (connection_description
            (websocket (uri ((scheme (ws)) (host (localhost)) (port PORT) (path /)))))
           (rpc_tag send-string) (rpc_version 1)) |}];
    (* successful connection *)
    let conn = read_new connection_pipe in
    print_when_connection_established_exn conn ~f:(fun (_ : Rpc.Connection.t) ->
      return ());
    let%bind () = dispatch_and_print (sprintf "ws://localhost:%d/" web_port) in
    [%expect {|
      New connection
      "OK from client" |}];
    return ())
;;
