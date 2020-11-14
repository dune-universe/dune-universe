module In_memory_buffer =
  Hvsock_lwt.Buffering.Make (Hvsock_lwt_unix.Preemptive_detach) (In_memory)
open Lwt.Infix

let send flow n =
  let buf = Cstruct.create 65536 in
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_char buf i 'X'
  done;
  let rec loop remaining =
    if remaining = 0 then Lwt.return_unit
    else
      let to_send = min (Cstruct.len buf) remaining in
      In_memory_buffer.writev flow [Cstruct.sub buf 0 to_send]
      >>= function
      | Error _ -> failwith "In_memory_buffer.writev"
      | Ok () -> loop (remaining - to_send)
  in
  loop n >>= fun () -> In_memory_buffer.shutdown_write flow

let read_all flow =
  let rec loop n =
    In_memory_buffer.read flow
    >>= function
    | Ok `Eof -> Lwt.return n
    | Ok (`Data m) -> loop (n + Cstruct.len m)
    | Error _ -> failwith "In_memory_buffer.read"
  in
  loop 0

let test_read_write to_write_client to_write_server =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server 5 ;
  let accepted_sock = ref None in
  let c = Condition.create () in
  let m = Mutex.create () in
  let accept_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        In_memory.close server ;
        Mutex.lock m ;
        accepted_sock := Some sock ;
        Condition.signal c ;
        Mutex.unlock m )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  Thread.join accept_thread ;
  Mutex.lock m ;
  let rec wait () =
    match !accepted_sock with
    | None -> Condition.wait c m ; wait ()
    | Some sock -> sock
  in
  let accepted_sock = wait () in
  Mutex.unlock m ;
  (* client is connected to accepted_sock *)
  Lwt_main.run
    (let open Lwt.Infix in
    let client_flow = In_memory_buffer.connect client in
    let server_flow = In_memory_buffer.connect accepted_sock in
    let client_reader = read_all client_flow in
    let server_reader = read_all server_flow in
    let client_writer = send client_flow to_write_client in
    let server_writer = send server_flow to_write_server in
    client_reader
    >>= fun client_read ->
    server_reader
    >>= fun server_read ->
    if client_read != to_write_server then
      failwith
        (Printf.sprintf "Client read %d but server wrote %d" client_read
           to_write_server) ;
    if server_read != to_write_client then
      failwith
        (Printf.sprintf "Server read %d but client wrote %d" server_read
           to_write_client) ;
    server_writer
    >>= fun () ->
    client_writer
    >>= fun () ->
    In_memory_buffer.close client_flow
    >>= fun () -> In_memory_buffer.close server_flow)
