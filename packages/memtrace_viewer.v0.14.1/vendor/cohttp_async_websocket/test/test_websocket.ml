open Core
open Async
open Cohttp_async_websocket

let%expect_test "Test tcp connection failure doesn't hang forever due to host lookup" =
  let uri = Uri.of_string "http://nonexistent:8000" in
  let%bind () =
    match%bind Client.create uri with
    | Ok _ -> assert false
    | Error e ->
      print_s [%message "" (e : Error.t)];
      Deferred.unit
  in
  [%expect
    {|
    (e
     (monitor.ml.Error
      (core_unix.ml.Inet_addr.Get_inet_addr nonexistent "host not found")
      ("<backtrace elided in test>"))) |}];
  return ()
;;

let%expect_test "Test tcp connection failure doesn't hang forever due to port connection \
                 refused"
  =
  let uri = Uri.of_string "http://127.0.0.1:0" in
  let%bind () =
    match%bind Client.create uri with
    | Ok _ -> assert false
    | Error e ->
      print_s [%message "" (e : Error.t)];
      Deferred.unit
  in
  [%expect
    {|
    (e
     (monitor.ml.Error
      (Unix.Unix_error "Connection refused" connect 127.0.0.1:PORT)
      ("<backtrace elided in test>" "Caught by monitor Tcp.close_sock_on_error"))) |}];
  return ()
;;

let ipv4_fds_open_in_this_process () =
  let pid = Unix.getpid () in
  let%bind file_descriptors =
    Process.run_lines ~prog:"/usr/sbin/lsof" ~args:[ "-p"; Pid.to_string pid ] ()
    >>| ok_exn
  in
  (* It's flaky to just look at all of the open FDs without any filtering. There are all
     kinds of different descriptors being used by async. Websocket connections use ipv4
     connections, so we filter down to those. *)
  let file_descriptors_for_IPv4_connections =
    List.filter file_descriptors ~f:(fun fd -> String.is_substring fd ~substring:"IPv4")
  in
  return file_descriptors_for_IPv4_connections
;;

module Test_server : sig
  type t

  val create : unit -> t Deferred.t
  val listening_on_address : t -> Socket.Address.Inet.t
  val close : t -> unit Deferred.t
end = struct
  type t = (Socket.Address.Inet.t, int) Tcp.Server.t

  let listening_on_address = Tcp.Server.listening_on_address
  let close t = Tcp.Server.close t

  let create () =
    Tcp.Server.create
      ~on_handler_error:`Raise
      Tcp.Where_to_listen.of_port_chosen_by_os
      (fun (_ : Socket.Address.Inet.t) (_ : Reader.t) writer ->
         Writer.write writer "INVALID";
         Writer.close writer)
  ;;
end

let%expect_test "access to headers from both client and server" =
  Log.Global.set_output [];
  let websocket_server =
    let handle_request ~inet:_ ~subprotocol:_ request =
      Cohttp_async_websocket.Server.On_connection.create
        ~set_response_headers:
          (Cohttp_async.Request.headers request) (* echo the headers back at the client *)
        ~should_overwrite_sec_accept_header:true
        (fun reader writer ->
           Pipe.close_read reader;
           Pipe.close writer;
           Pipe.closed reader)
      |> return
    in
    Cohttp_async_websocket.Server.create
      ~non_ws_request:(fun ~body:_ -> failwith "got a request that wasn't websocket!")
      handle_request
  in
  let%bind http_server =
    Cohttp_async.Server.create_expert
      Tcp.Where_to_listen.of_port_chosen_by_os
      ~on_handler_error:`Raise
      websocket_server
  in
  let port = Cohttp_async.Server.listening_on http_server in
  let headers = Cohttp.Header.of_list [ "top-secret", "this is the value" ] in
  let url = Uri.of_string (sprintf "http://localhost:%d" port) in
  let%bind response, reader, writer =
    Cohttp_async_websocket.Client.create ~headers url >>| Or_error.ok_exn
  in
  Pipe.close_read reader;
  Pipe.close writer;
  let%bind () = Pipe.closed reader in
  response
  |> Cohttp_async.Response.headers
  |> Fn.flip Cohttp.Header.get "top-secret"
  |> Option.value_exn
  |> print_endline;
  [%expect {| this is the value |}];
  return ()
;;

let run_test ~protocol =
  Log.Global.set_output [];
  let%bind server = Test_server.create () in
  let address = Test_server.listening_on_address server in
  let port = Socket.Address.Inet.port address in
  let uri = Uri.make ~scheme:protocol ~host:"127.0.0.1" ~port () in
  let%bind fds_before_websocket = ipv4_fds_open_in_this_process () in
  let%bind () =
    match%bind Client.create uri with
    | Ok (_ : Cohttp_async.Response.t * string Pipe.Reader.t * string Pipe.Writer.t) ->
      raise_s [%message "This test expects the connection to fail, but it succeeded"]
    | Error e ->
      print_s [%sexp (e : Error.t)];
      return ()
  in
  let%bind fds_after_websocket = ipv4_fds_open_in_this_process () in
  let total_fd_leak =
    List.length fds_after_websocket - List.length fds_before_websocket
  in
  if total_fd_leak <> 0
  then
    raise_s
      [%message
        "fds unexpectedly changed"
          (fds_before_websocket : string list)
          (fds_after_websocket : string list)];
  print_s [%message "" (total_fd_leak : int)];
  Test_server.close server
;;

let%expect_test "Test no file descriptor leak in client leak on invalid response with no \
                 ssl"
  =
  let%bind () = run_test ~protocol:"ws" in
  [%expect
    {|
    ("Bad response to websocket request"
     (response (Invalid "Malformed response first line: INVALID")))
    (total_fd_leak 0) |}];
  return ()
;;
