open OUnit
module U = Unix

let setup_test_server () =
  let socket = U.socket U.PF_INET U.SOCK_STREAM 0 in
  U.setsockopt socket U.TCP_NODELAY true;
  U.set_close_on_exec socket;
  U.bind socket (U.ADDR_INET(U.inet_addr_any, 0));
  U.listen socket 50;
  let recv_buf = Buffer.create 512 in
  let current_accept_socket = ref None in
  ignore (
    Thread.create (fun _ ->
      let (cs, sockaddr) = U.accept socket in
      current_accept_socket := Some cs; 
      let in_ch = U.in_channel_of_descr cs in
      let rec _loop () =
        let buf = String.create 1024 in
        let len = input in_ch buf 0 (String.length buf) in
        if len = 0 then close_in in_ch
        else (
          Buffer.add_string recv_buf (String.sub buf 0 len);
          _loop ()
        )
        in
      _loop ()
    ) ()
  );
  match U.getsockname socket with
  | U.ADDR_INET (_, port) ->
      (socket, port, 
        (fun () -> 
          match !current_accept_socket with
          Some cs -> cs | None -> failwith "no accept_socket"),
        (fun () -> Buffer.contents recv_buf))
  | _ -> failwith "unexpected socket domain"

let test_inet_sender_write () =
  (* setup *)
  let (socket, port, get_accept_socket, get_recv_data) = setup_test_server () in
  let sender = Stream_sender.create (Stream_sender.INET("localhost", port)) 3 in
  (* first msg *)
  let msg1 = "helloworld" in
  let msg_len = String.length msg1 in
  assert_equal (Some msg_len) (Stream_sender.write sender msg1 0 msg_len);
  (* second msg *)
  let msg2 = "abracadabra" in
  let msg_len = String.length msg2 in
  assert_equal (Some msg_len) (Stream_sender.write sender msg2 0 msg_len);
  (* close server and then third msg *)
  U.sleep 1;
  let accept_socket = get_accept_socket () in
  U.close accept_socket;
  U.close socket;
  U.sleep 1;
  let msg = "foobar" in
  let msg_len = String.length msg in
  assert_equal None (Stream_sender.write sender msg 0 msg_len);
  assert_equal (msg1 ^ msg2) (get_recv_data ())

let suite =
  "Fluent_logger tests" >:::
    ["test_inet_sender_write"         >:: test_inet_sender_write]

let () =
  Printexc.record_backtrace true;
  ignore (run_test_tt_main suite)

