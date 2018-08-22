open Lwt.Infix
open Cmdliner       


module X = Lwt_rsocket 
module B = Lwt_rsocket.Bytes

(* (* You can run the same code with tcip sockets too *)
module X = struct
  include Lwt_unix
  let identifier fd =
    let ufd = unix_file_descr fd in
    let (r:int) = Obj.magic ufd in
    r
end
module B = Lwt_bytes
 *)

external set32_prim : Lwt_bytes.t -> int -> int32 -> unit = "%caml_bigstring_set32"
external get32_prim : Lwt_bytes.t -> int -> int32 = "%caml_bigstring_get32"

let really_send client_fd bytes off len =
    let rec loop count off todo =
      if todo = 0
      then Lwt.return count
      else
        B.send client_fd bytes off todo [] >>= fun sent ->
        loop (count + 1) (off + sent) (todo - sent)
    in
    loop 0 off len >>= function
    | 0 | 1 -> Lwt.return_unit
    | n -> Lwt_log.debug_f "send (%i bytes) in %i steps" len n

let really_read client_fd bytes off len =
  let rec loop count off todo =
    if todo = 0
    then Lwt.return count
    else 
      B.recv client_fd bytes off todo [] >>= fun received ->
      loop (count +1) (off + received) (todo - received)
  in
  loop 0 off len >>= function
  | 0 | 1 -> Lwt.return_unit
  | n -> Lwt_io.printf "received (%i bytes_ in %i steps" len n


let client host port len n =
  let t = 
    Lwt_log.debug_f "client (%s,%i)%!" host port >>= fun () ->
    let addr = Unix.inet_addr_of_string host in
    let fd = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let sa = Unix.ADDR_INET (addr,port) in
    (*Lwt_log.debug_f "connecting%!" >>= fun () -> *)
    X.connect fd sa >>= fun () ->
    (* Lwt_log.debug_f "connected%!" >>= fun () -> *)
    let rest = len - 4 in
    let bytes_out = B.create len in
    let () = set32_prim bytes_out 0 (rest |> Int32.of_int) in
    let bytes_in  = B.create len in
    let rec loop n =
      if n = 0
      then Lwt.return_unit
      else
        (if n mod 1000 = 0
         then Lwt_log.debug_f "%i\n" n
         else Lwt.return_unit
        )
        >>= fun () ->
        really_send fd bytes_out 0 len >>= fun () ->
        really_read fd bytes_in 0 len >>= fun () ->
        assert (bytes_out = bytes_in);
        loop (n-1)
    in
    loop n >>= fun () ->
    X.close fd
  in
  Lwt_main.run t
                                     
let server host port buffer_size =
  let t = 
    Lwt_log.debug_f "server (%s,%i)%!" host port >>= fun () ->
    let addr = Unix.inet_addr_of_string host in
    let server_socket = X.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt_log.debug_f "got a socket:(%i)" (X.identifier server_socket) >>= fun () ->
    let sa = Unix.ADDR_INET (addr, port) in
    let () = X.setsockopt server_socket Unix.SO_REUSEADDR true in 
    Lwt_log.debug_f "set REUSEADDR true%!" >>= fun () ->
    (* *)
    let () = X.bind server_socket sa in
    Lwt_log.debug_f "bind ok%!" >>= fun () ->
    let () = X.listen server_socket 40 in
    Lwt_log.debug_f "listen ok%!" >>= fun () ->

    
    let protocol client_fd =
      let bytes = B.create buffer_size in
      let id = X.identifier client_fd in
      let rec loop () =
        Lwt_log.debug_f "(%i) protocol: recv%!" id >>= fun () -> 
        B.recv client_fd bytes 0 4      [] >>= fun received ->
        Lwt_log.debug_f "(%i) wanted 4, got %i" id received >>= fun () -> 
        assert (received = 4);
        let rest = get32_prim bytes 0 |> Int32.to_int in
        (* Lwt_log.debug_f "rest: %i" rest >>= fun () -> *)
        really_read client_fd bytes 4 rest >>= fun () ->
        let total = rest + 4 in
        (* Lwt_log.debug_f "protocol: received (%i)%!" (rest + 4) >>= fun () ->
        Lwt_log.debug_f "protocol: send (%i)%!" total  >>= fun () -> *)
        really_send client_fd bytes 0 total >>= fun () ->
        loop ()
      in
      loop ()
    in
    let detach_safe_connection fd peer protocol =
      let id = X.identifier fd in
      let f () =
        Lwt.catch
          (fun () ->
            Lwt_log.debug_f "(%i) running protocol%!" id  >>= fun () ->
            protocol fd
          )
          (fun ex ->
            Lwt_log.debug_f "(%i) end of protocol:%s%!" id (Printexc.to_string ex)
            >>= fun () ->
            X.close fd)
      in
      Lwt.ignore_result (f ())
    in
    let rec loop () =
      Lwt_log.debug_f "pre_accept%!" >>= fun () ->
      X.accept server_socket >>= fun (fd, peer) ->
      Lwt_log.debug_f "accepted (%i) detaching%!" (X.identifier fd) >>= fun () ->
      let () = detach_safe_connection fd peer protocol in
      loop ()
    in
    Lwt.finalize
      (fun () -> loop ())
      (fun () -> X.close server_socket)
  in
  Lwt_main.run t

let accept_bug host port =
  
  let t =
    Lwt_log.debug_f "accept_bug (%s,%i)%!" host port >>= fun () ->
    let sa = Unix.ADDR_INET(Unix.inet_addr_of_string host, port) in
    let domain = Unix.domain_of_sockaddr sa in
    let typ = Unix.SOCK_STREAM in
    let connect () =
      let fd= X.socket domain typ 0 in
      Lwt_log.debug_f "(%i)" (X.identifier fd) >>= fun ()->
      X.connect fd sa >>= fun () ->
      Lwt.return fd
    in
    (*
  let prologue_bytes = Asd_client.make_prologue "aLbA" 1l None in
  let prologue_len = Bytes.length prologue_bytes in
     *)
    let buffer = Bytes.create 128 in
     
    connect () >>= fun fd0 ->
    Lwt_log.debug_f "connected 1%!" >>= fun () ->
    connect () >>= fun fd1 -> 
    
    let d = 10.0 in
    Lwt_log.debug_f "connected.... now sleeping %f%!" d >>= fun () ->
    Lwt_unix.sleep d >>= fun () -> 
    
    Lwt_log.debug_f "sending over fd0%!" >>= fun ()->
    X.send fd0 buffer 0 (String.length buffer) [] >>= fun sent ->
    assert (sent = String.length buffer);    
    (*
    Lwt_rsocket.recv fd0 buffer 0 4 [] >>= fun read ->
    assert (read = 4);
    let rc = get32_prim buffer 0 in
    assert (rc = 0l);
    Lwt_rsocket.recv fd0 buffer 0 4 [] >>= fun read ->
    assert (read = 4);

    let len = get32_prim buffer 0 |> Int32.to_int in
    Lwt_rsocket.recv fd0 buffer 0 len [] >>= fun read ->
    assert(read = len);
    (* GetDiskUsage *)
    let get_disk_usage = 10l in
    set32_prim buffer 0 4l ;
    set32_prim buffer 4 get_disk_usage;
    Lwt_rsocket.send fd0 buffer 0 8 [] >>= fun sent ->
    assert (sent = 8);
    Lwt_rsocket.recv fd0 buffer 0 4 [] >>= fun read ->
    assert (read = 4);
    let response_length = get32_prim buffer 0 |> Int32.to_int in
    Lwt_rsocket.recv fd0 buffer 0 response_length [] >>= fun read ->
    assert (read = response_length);
     *)
    X.close fd0 >>= fun () ->
    X.close fd1 >>= fun () ->
    Lwt.return_unit
  in
  Lwt_main.run t
               
let () =
  let () =
    Lwt_log.default :=
      Lwt_log.channel 
        ~channel:Lwt_io.stdout
        ~close_mode:`Keep
        ~template:"$(date).$(milliseconds) $(message)"
        ()
  in
  let () = Lwt_log_core.append_rule "*" Lwt_log_core.Debug in
  let () = Sys.set_signal Sys.sigpipe Sys.Signal_ignore in
  let () =
    let open Rsocket.Version in
    Printf.printf "ordma_version (%i,%i,%i,%s)\n" major minor patch git_revision
  in
  let engine = new Lwt_rsocket.rselect in
  Lwt_engine.set engine; 
  let () = Printf.printf "set engine to `rselect`\n%!" in
  let default_cmd =
    let copts_sect = "COMMON OPTIONS" in
    let copts_t = Term.pure "x" (* ?? *) in
    let doc = "test" in
    let man = [] in
    Term.(ret (pure (fun _ -> `Help (`Pager,None)) $copts_t)),
    Term.info "test" ~sdocs:copts_sect ~doc ~man
  in
  let host =
    Arg.(value
         & opt string "192.168.1.1"
         & info ["h";"host"] ~docv:"HOST" ~doc:"host"
    )
  in
  let port default =
    Arg.(value
         & opt int default
         & info ["p";"port"] ~docv:"PORT" ~doc:"port"
    )
  in
  let len default =
    Arg.(value
         & opt int default
         & info ["l";"length"] ~docv:"LENGTH" ~doc:"length of message"
    )
  in
  let buffer_size default =
    Arg.(value
         & opt int default
         & info ["b";"buffer-size"] ~docv:"BUFER_SIZE" ~doc:"size of server side buffer"
    )
  in
  let n =
    Arg.(value
         & opt int 10
         & info ["n"] ~docv:"N" ~doc:"number of iterations"
    )
  in
  let client_cmd =
    let t = Term.(pure client
                  $ host
                  $ port 10_000
                  $ len 4096
                  $ n)
    in
    let info =
      Term.info "client" ~doc:"run echo client"
    in
    t,info
  in
  let server_cmd =
    let t = Term.(pure server
                  $ host
                  $ port 10_000
                  $ buffer_size 4096
            )
    in
    let info =
      Term.info "server" ~doc:"start echo server"
    in
    t,info
  in
  let accept_bug_cmd =
    let t = Term.(pure accept_bug
                  $ host
                  $ port 10_000
            )
    in
    let info = Term.info "accept_bug" ~doc:"client side of accept bug" in
    t, info
  in
  let cmds =
    [client_cmd;
     server_cmd;
     accept_bug_cmd;
    ]
  in
  match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | _ -> exit 0

