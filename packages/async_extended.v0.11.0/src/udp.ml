open Core
open Async
module Socket = Socket

module Udp_socket : sig
(*  val sendto
    :  ([< `Unconnected | `Bound ], 'addr) Socket.t
    -> buf:string
    -> pos:int
    -> len:int
    -> addr:'addr
    -> int Deferred.t
*)
  val recvfrom
    :  ([ `Bound ], Socket.Address.Inet.t) Socket.t
    -> buf:Bytes.t
    -> pos:int
    -> len:int
    -> (int * Socket.Address.Inet.t) Deferred.t
end = struct
  open Socket


  (*
  let sendto sock ~buf ~pos ~len ~addr =
    let fd = fd sock in
    let addr = Socket.Address.to_sockaddr addr in
    Unix.Fd.ready_to fd `Write >>| function
    | `Closed | `Bad_fd ->
        failwithf "sendto on bad file descriptor %s" (Unix.Fd.to_string fd) ()
    | `Ready ->
      Core.Unix.sendto (Unix.Fd.file_descr_exn fd) ~buf ~pos ~len ~addr ~mode:[]
  ;;
  *)

  let recvfrom sock ~buf ~pos ~len =
    let fd = fd sock in
    Unix.Fd.ready_to fd `Read >>| function
    | `Closed | `Bad_fd ->
        failwithf "recvfrom on bad file descriptor %s" (Unix.Fd.to_string fd) ()
    | `Ready ->
        let i, addr =
          Core.Unix.recvfrom (Unix.Fd.file_descr_exn fd) ~buf ~pos ~len ~mode:[]
        in
        let addr =
          (* this should only ever be called on inet sockets *)
          match addr with
          | Core.Unix.ADDR_INET (a, i) -> `Inet (a, i)
          | _ -> assert false
        in
        (i, addr)
  ;;
end

let try_with = Monitor.try_with

let shutdown_close sock =
  don't_wait_for
    ((try Socket.shutdown sock `Both with | _ -> ());
    (try_with (fun () -> Unix.close (Socket.fd sock))) >>| fun _ -> ())
;;

let connect socket_type ~addr =
  let timeout = after (sec 10.) in
  let sock = Socket.create socket_type in
  Unix.set_close_on_exec (Socket.fd sock);
  try_with (fun () ->
    Socket.connect_interruptible sock addr ~interrupt:timeout)
  >>= function
    | Ok (`Ok sock) -> return sock
    | Ok `Interrupted ->
      shutdown_close sock;
      failwith "connection timed out"
    | Error e ->
      shutdown_close sock;
      raise (Monitor.extract_exn e)
;;

let using ?buffer_age_limit sock ~f =
  Unix.set_close_on_exec (Socket.fd sock);
  let r = Reader.create ~buf_len:(10 * 1024 * 1024) (Socket.fd sock) in
  let w = Writer.create ?buffer_age_limit (Socket.fd sock) in
  try_with (fun () ->
    begin
      Stream.next (Monitor.detach_and_get_error_stream (Writer.monitor w))
      >>> function
        | Stream.Nil -> ()
        | Stream.Cons (e, _) -> raise e
    end;
    f r w) >>= fun res ->
  Clock.with_timeout (sec 30.) (Writer.flushed w) >>= fun _ ->
  (try Socket.shutdown sock `Both with | _ -> ());
  try_with (fun () -> Writer.close w
    ~force_close:(after (sec 30.))) >>= fun _ ->
  try_with (fun () -> Reader.close r)            >>| fun _ ->
  match res with
  | Ok res -> res
  | Error e ->
    raise (Monitor.extract_exn e)
;;

let using' ?buffer_age_limit sock ~f = ignore (using ?buffer_age_limit sock ~f)

let udp_server ~addr ~port ~f =
  Deferred.create (fun server_ready ->
    Unix.Inet_addr.of_string_or_getbyname addr >>> fun addr ->
    let addr = Socket.Address.Inet.create addr ~port in
    let sock = Socket.create Socket.Type.udp in
    using' sock ~f:(fun _r _w ->
      Socket.setopt sock Socket.Opt.reuseaddr true;
      Socket.bind sock addr >>= fun sock ->
      Ivar.fill server_ready ();
      let buf_len = 65535 in
      let buf = Bytes.create buf_len in
      let rec receive_loop () =
        (Udp_socket.recvfrom sock ~buf ~pos:0 ~len:buf_len >>| fun (len,addr) ->
        (addr, Bytes.To_string.sub buf ~pos:0 ~len)) >>> fun (addr,data) ->
        f addr data >>>
        receive_loop
      in
      receive_loop ();
      Deferred.never ()))
;;

let client ?buffer_age_limit socket_type ~addr ~f =
  connect socket_type ~addr >>= fun sock ->
  using ?buffer_age_limit sock ~f
;;


let udp_client ~addr ~port ~f =
  Unix.Inet_addr.of_string_or_getbyname addr >>= fun addr ->
  let addr = Socket.Address.Inet.create addr ~port in
  client Socket.Type.udp ~addr ~f:(fun _ w -> f w)
;;

let tcp_server_generic
    ?max_connections ~addr ~stype ~on_client_error ~f
    ?buffer_age_limit () =
  let num_connections = ref 0 in
  Deferred.create (fun server_ready ->
    let sock = Socket.create stype in
    using' ?buffer_age_limit sock ~f:(fun _r _w ->
      Socket.setopt sock Socket.Opt.reuseaddr true;
      Socket.bind sock addr >>= fun bound_sock ->
      let server_sock = Socket.listen bound_sock ~backlog:500 in
      Ivar.fill server_ready ();
      let rec accept_loop () =
        Socket.accept server_sock
        >>> function
        | `Socket_closed -> ()
        | `Ok (client_sock, addr) ->
        let below_max_connections =
          match max_connections with
          | None -> true
          | Some max_connections -> !num_connections < max_connections
        in
        if below_max_connections
        then begin
          num_connections := !num_connections + 1;
          accept_loop ();
          begin
            (try_with (fun () ->
              using ?buffer_age_limit client_sock ~f:(fun r w -> f addr r w)) >>|
                function
                  | Ok () -> ()
                  | Error e ->
                    match on_client_error with
                    | `Ignore -> ()
                    | `Call f -> f addr e)
            >>> fun () -> num_connections := !num_connections - 1;
          end
        end
        else begin
          (try Socket.shutdown sock `Both with _ -> ());
          accept_loop ()
        end
      in
      accept_loop ();
      Deferred.never ()))

let tcp_server_unix ?max_connections ~addr ~on_client_error ~f
?buffer_age_limit () =
  tcp_server_generic
    ?max_connections
    ~stype:Socket.Type.unix
    ~addr
    ~on_client_error
    ~f
    ?buffer_age_limit
    ()
