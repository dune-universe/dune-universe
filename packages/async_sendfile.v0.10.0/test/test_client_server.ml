open! Core
open Async
open Expect_test_helpers

let delivery_unit = Byte_units.create `Bytes 1.

let payload = "Testing"

let server file =
  Tcp.Server.create
    ~on_handler_error:`Raise
    (Tcp.Where_to_listen.of_port_chosen_by_os)
    (fun _ _ w ->
       Async_sendfile.sendfile ~delivery_unit ~socket_fd:(Writer.fd w) ~file ()
       >>| ok_exn)
;;

let client where_to_connect =
  let buffer = Buffer.create (String.length payload) in
  let%bind () =
    Tcp.with_connection where_to_connect (fun _ r _ ->
      Reader.lines r
      |> Pipe.iter_without_pushback ~f:(Buffer.add_string buffer))
  in
  return (Buffer.contents buffer)
;;

let%expect_test _ =
  with_temp_dir (fun tmpdir ->
    let file = tmpdir ^/ "test_file" in
    let%bind () = Writer.save file ~contents:payload in
    let%bind server = server file in
    let%bind payload' =
      client
        (Tcp.Where_to_connect.of_host_and_port
           { host = "localhost"; port = Tcp.Server.listening_on server })
    in
    [%test_result: string] ~expect:payload payload';
    Deferred.unit)
;;
