module In_memory_buffer =
  Hvsock_lwt.Buffering.Make (Hvsock_lwt_unix.Preemptive_detach) (In_memory)

let test_buffered_write_read () =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server 5 ;
  let received = ref "" in
  let server_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        let buf = Cstruct.create 10 in
        let n = In_memory.read_into sock buf in
        (received := Cstruct.(to_string @@ sub buf 0 n)) ;
        In_memory.close sock )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  Lwt_main.run
    (let open Lwt.Infix in
    let flow = In_memory_buffer.connect client in
    In_memory_buffer.writev flow [Cstruct.of_string "hello\n"]
    >>= function
    | Error _ -> failwith "In_memory_buffer.writev"
    | Ok () ->
        In_memory_buffer.shutdown_write flow
        >>= fun () ->
        Thread.join server_thread ;
        Alcotest.(check string) "Check message" "hello\n" !received ;
        In_memory_buffer.close flow
        >>= fun () -> In_memory.close server ; Lwt.return_unit)

let interesting_sizes =
  [ 0
  ; 1
  ; 4
  ; 4095
  ; 4096
  ; 4097
  ; 4098
  ; 4099
  ; 5000
  ; 5001
  ; 5002
  ; 1048575
  ; 1048576
  ; 1048577 ]

let rec cross xs ys =
  match xs with
  | [] -> []
  | x :: xs -> List.map (fun y -> (x, y)) ys @ cross xs ys

let test_buffering =
  [("write then read", `Quick, test_buffered_write_read)]
  @ List.map
      (fun (x, y) ->
        ( Printf.sprintf "write %d, write %d" x y
        , `Quick
        , fun () -> Buffering.test_read_write x y ) )
      (cross interesting_sizes interesting_sizes)

let test_accept_connect () =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server 5 ;
  let server_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        In_memory.close sock )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  Thread.join server_thread ;
  In_memory.close server

let test_write_read () =
  let server = In_memory.create () in
  In_memory.bind server () ;
  In_memory.listen server 5 ;
  let received = ref "" in
  let server_thread =
    Thread.create
      (fun () ->
        let sock, _ = In_memory.accept server in
        let buf = Cstruct.create 10 in
        let n = In_memory.read_into sock buf in
        (received := Cstruct.(to_string @@ sub buf 0 n)) ;
        In_memory.close sock )
      ()
  in
  let client = In_memory.create () in
  In_memory.connect client () ;
  let n = In_memory.writev client [Cstruct.of_string "hello\n"] in
  Alcotest.(check int) "writev" 6 n ;
  In_memory.shutdown_write client ;
  Thread.join server_thread ;
  Alcotest.(check string) "read_into" "hello\n" !received ;
  In_memory.close server ;
  In_memory.close client

let test_simulated_socket =
  [ ("accept connect", `Quick, test_accept_connect)
  ; ("write then read", `Quick, test_write_read) ]

let () =
  Logs.set_reporter (Logs_fmt.reporter ()) ;
  Alcotest.run "hvsock"
    [("socket", test_simulated_socket); ("buffering", test_buffering)]
