open! Core
open! Async
open! Import

module Query_and_response = struct
  type t = string [@@deriving bin_io, sexp]
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
    ~implementations:[ Rpc.Rpc.implement rpc (fun () str -> return str) ]
;;

let serve () =
  Rpc_websocket.Rpc.serve
    ~where_to_listen:Tcp.Where_to_listen.of_port_chosen_by_os
    ~implementations
    ~initial_connection_state:
      (fun () initiated_from (_ : Socket.Address.Inet.t) (_ : Rpc.Connection.t) ->
         print_s [%sexp (initiated_from : Rpc_websocket.Rpc.Connection_initiated_from.t)];
         ())
    ()
;;

let rpc_client server =
  let uri_of_server server =
    let port = Cohttp_async.Server.listening_on server in
    Uri.make ~host:"localhost" ~port ()
  in
  Rpc_websocket.Rpc.client
    ~headers:(Cohttp.Header.of_list [ "a-key", "a-value" ])
    (uri_of_server server)
;;

module Expect_test_config = Deferred.Or_error.Expect_test_config

let%expect_test "roundtrip a string over websockets" =
  let open Deferred.Or_error.Let_syntax in
  let%bind server = serve () |> Deferred.ok in
  let%bind conn = rpc_client server in
  let string_sent = "I sent this" in
  let%bind got = Rpc.Rpc.dispatch rpc conn string_sent in
  let sanitize_port s =
    let port_str = Cohttp_async.Server.listening_on server |> Int.to_string in
    Accessor.map Accessor_base.Sexp.atoms s ~f:(fun atom ->
      String.substr_replace_all atom ~pattern:port_str ~with_:"PORT")
  in
  [%test_result: string] ~expect:string_sent got;
  let%bind output = [%expect.output] in
  print_s (sanitize_port (Sexp.of_string output));
  [%expect
    {|
    (Websocket_request
     ((headers
       ((a-key a-value) (connection Upgrade) (host localhost:PORT)
        (origin ws://localhost:PORT) (sec-websocket-key n6INw7ch3mJP8/EK54zJFw==)
        (sec-websocket-version 13) (upgrade websocket)
        (user-agent ocaml-cohttp/1.0.2)))
      (meth GET) (resource /) (version HTTP_1_1) (encoding Unknown))) |}]
;;
