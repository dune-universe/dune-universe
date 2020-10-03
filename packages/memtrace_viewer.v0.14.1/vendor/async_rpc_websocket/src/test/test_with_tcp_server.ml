open! Core
open! Async
open! Import

module Query_and_response = struct
  type t = unit [@@deriving bin_io, sexp]
end

let rpc : (Query_and_response.t, Query_and_response.t) Rpc.Rpc.t =
  Rpc.Rpc.create
    ~name:"test"
    ~version:0
    ~bin_query:Query_and_response.bin_t
    ~bin_response:Query_and_response.bin_t
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Raise
    ~implementations:
      [ Rpc.Rpc.implement rpc (fun () () ->
          print_s [%message "got query"];
          Deferred.unit)
      ]
;;

let send_websocket_request port =
  let%bind (_ : Cohttp_async.Response.t), reader, writer =
    let uri = Uri.make ~host:"localhost" ~port () in
    Cohttp_async_websocket.Client.create uri |> Deferred.Or_error.ok_exn
  in
  let%bind reader = Reader.of_pipe (Info.of_string "websocket-reader") reader in
  let%bind writer, `Closed_and_flushed_downstream on_close =
    Writer.of_pipe (Info.of_string "websocket-writer") writer
  in
  let transport =
    let max_message_size = Byte_units.(bytes_int_exn (of_megabytes 100.)) in
    Rpc.Transport.of_reader_writer ~max_message_size reader writer
  in
  don't_wait_for
    (let%bind () = Deferred.ignore_m on_close in
     let%map () = Writer.close writer
     and () = Rpc.Transport.close transport in
     ());
  Async_rpc_kernel.Rpc.Connection.with_close
    ~connection_state:(const ())
    transport
    ~dispatch_queries:(fun conn ->
      Deferred.Or_error.ignore_m (Rpc.Rpc.dispatch rpc conn ()))
    ~on_handshake_error:`Raise
;;

let do_not_perform_global_logging () = Log.Global.set_output []

let%expect_test "serve_with_tcp_server" =
  do_not_perform_global_logging ();
  let rpc_server, web_server =
    Rpc_websocket.Rpc.serve_with_tcp_server
      ~where_to_listen_for_tcp:Tcp.Where_to_listen.of_port_chosen_by_os
      ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
      ~initial_connection_state:
        (fun ()
          (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
          (_ : Socket.Address.Inet.t)
          (_ : Rpc.Connection.t) -> ())
      ~implementations
      ()
  in
  let%bind rpc_server = rpc_server
  and web_server = web_server in
  let rpc_port = Tcp.Server.listening_on rpc_server in
  let web_port = Cohttp_async.Server.listening_on web_server in
  let%bind () =
    Rpc.Connection.with_client
      (Tcp.Where_to_connect.of_host_and_port
         (Host_and_port.create ~host:"localhost" ~port:rpc_port))
      (fun connection -> Rpc.Rpc.dispatch_exn rpc connection ())
    |> Deferred.map ~f:(Result.map_error ~f:Error.of_exn)
    |> Deferred.Or_error.ok_exn
  in
  [%expect {| "got query" |}];
  let%bind () = send_websocket_request web_port >>| ok_exn in
  [%expect {| "got query" |}];
  return ()
;;

let%test_module "TCP vs Websocket Pipe Pushback" =
  (module struct
    module Query = struct
      type t = unit [@@deriving bin_io]
    end

    module Response = struct
      type t = string [@@deriving bin_io]
    end

    let rpc =
      Rpc.Pipe_rpc.create
        ~client_pushes_back:()
        ~name:"rpc"
        ~version:0
        ~bin_query:Query.bin_t
        ~bin_response:Response.bin_t
        ~bin_error:Error.bin_t
        ()
    ;;

    let rw_of_sock socket =
      let reader = Reader.create ~buf_len:2 (Socket.fd socket) in
      let writer = Writer.create ~buf_len:2 (Socket.fd socket) in
      reader, writer
    ;;

    let implementations () =
      let ivar = Ivar.create () in
      let implementation =
        Rpc.Pipe_rpc.implement rpc (fun () () ->
          let pipe_r, pipe_w = Pipe.create () in
          Pipe.set_size_budget pipe_w 1;
          Pipe.set_size_budget pipe_r 1;
          Ivar.fill ivar pipe_w;
          return (Ok pipe_r))
      in
      ( Ivar.read ivar
      , Rpc.Implementations.create_exn
          ~implementations:[ implementation ]
          ~on_unknown_rpc:`Raise )
    ;;

    module Transport = struct
      module Kind = struct
        type t =
          | Tcp
          | Websocket
        [@@deriving enumerate, sexp_of]
      end

      let create ~implementations (kind : Kind.t) =
        let ok_exn_result x = x >>| Or_error.of_exn_result |> Deferred.Or_error.ok_exn in
        match kind with
        | Tcp ->
          let%bind server =
            Tcp.Server.create_sock
              ~on_handler_error:`Raise
              Tcp.Where_to_listen.of_port_chosen_by_os
              (fun (_ : Socket.Address.Inet.t) socket ->
                 let reader, writer = rw_of_sock socket in
                 let%bind connection =
                   Rpc.Connection.create
                     reader
                     writer
                     ~implementations
                     ~connection_state:(fun (_ : Rpc.Connection.t) -> ())
                   |> ok_exn_result
                 in
                 Rpc.Connection.close_finished connection)
          in
          let where_to_connect =
            Tcp.Where_to_connect.of_host_and_port
              (Host_and_port.create
                 ~host:"localhost"
                 ~port:(Tcp.Server.listening_on server))
          in
          let%bind client =
            let%bind socket = Tcp.connect_sock where_to_connect in
            let reader, writer = rw_of_sock socket in
            Rpc.Connection.create reader writer ~connection_state:(const ())
            |> ok_exn_result
          in
          return client
        | Websocket ->
          let%bind server =
            Rpc_websocket.Rpc.serve
              ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
              ~initial_connection_state:
                (fun ()
                  (_ : Rpc_websocket.Rpc.Connection_initiated_from.t)
                  (_ : Socket.Address.Inet.t)
                  (_ : Rpc.Connection.t) -> ())
              ~implementations
              ()
          in
          let port = Cohttp_async.Server.listening_on server in
          let%bind (_ : Cohttp_async.Response.t), reader, writer =
            let uri = Uri.make ~host:"localhost" ~port () in
            Cohttp_async_websocket.Client.create uri |> Deferred.Or_error.ok_exn
          in
          let%bind reader = Reader.of_pipe (Info.of_string "websocket-reader") reader in
          let%bind writer, (_ : [ `Closed_and_flushed_downstream of unit Deferred.t ]) =
            Writer.of_pipe (Info.of_string "websocket-writer") writer
          in
          let transport =
            let max_message_size = Byte_units.(bytes_int_exn (of_megabytes 100.)) in
            Rpc.Transport.of_reader_writer ~max_message_size reader writer
          in
          Async_rpc_kernel.Rpc.Connection.create ~connection_state:(const ()) transport
          |> ok_exn_result
      ;;
    end

    type t =
      { server_pipe_writer : string Pipe.Writer.t
      ; client_pipe_reader : string Pipe.Reader.t
      }

    let create transport_kind =
      let pipe, implementations = implementations () in
      let%bind client = Transport.create transport_kind ~implementations in
      let%bind client_pipe_reader, (_ : Rpc.Pipe_rpc.Metadata.t) =
        Rpc.Pipe_rpc.dispatch_exn rpc client ()
      in
      let%bind pipe = pipe in
      return { server_pipe_writer = pipe; client_pipe_reader }
    ;;

    let run kind =
      let%bind { server_pipe_writer; client_pipe_reader } = create kind in
      don't_wait_for
        (Pipe.iter client_pipe_reader ~f:(fun (_ : string) -> Deferred.never ()));
      let arbitrary_large_number_that_seems_to_induce_pushback = 250_000 in
      List.init arbitrary_large_number_that_seems_to_induce_pushback ~f:(const ())
      |> List.iter ~f:(fun () -> Pipe.write_without_pushback server_pipe_writer "");
      let this_was_long_enough_to_witness_real_pushback = Time.Span.of_sec 5. in
      match%map
        Clock.with_timeout
          this_was_long_enough_to_witness_real_pushback
          (Pipe.write server_pipe_writer "hi")
      with
      | `Timeout -> `Pushback_occurred
      | `Result () -> `No_pushback
    ;;

    let test_transport kind =
      Expect_test_helpers_async.require_does_not_raise_async
        ~cr:CR_someday
        [%here]
        (fun () ->
           match%map run kind with
           | `Pushback_occurred -> ()
           | `No_pushback ->
             raise_s [%message "Expected some pushback" (kind : Transport.Kind.t)])
    ;;

    let%expect_test "TCP Pushback" =
      let%bind () = test_transport Transport.Kind.Tcp in
      [%expect {| |}];
      return ()
    ;;

    let%expect_test "Websocket Pushback" =
      let%bind () = test_transport Transport.Kind.Websocket in
      [%expect
        {|
        ("unexpectedly raised" ("Expected some pushback" (kind Websocket))) |}];
      return ()
    ;;
  end)
;;
