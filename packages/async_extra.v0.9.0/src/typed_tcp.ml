open Core
open Import
open Typed_tcp_intf

module type Arg = Arg
module type Binable_t = Binable_t
module type S = S

module Make (Arg : Arg) () = struct
  module Client_message = Arg.Client_message
  module Server_message = Arg.Server_message
  module Transport = Arg.Transport
  module Client_id = Unique_id.Int63 ()

  module Server_read_result = struct
    type t =
      | Connect       of Client_id.t
      | Disconnect    of Client_id.t * Sexp.t
      | Denied_access of string
      | Data          of Client_id.t * Client_message.t
    [@@deriving sexp]

  end

  type client =
    { close     : exn -> unit
    ; transport : Transport.t
    ; addr      : Unix.Inet_addr.t
    ; port      : int
    }

  type t =
    { verbose           : bool
    ; log_disconnects   : bool
    ; clients           : client Client_id.Table.t
    ; result_reader     : Server_read_result.t Pipe.Reader.t
    ; result_writer     : Server_read_result.t Pipe.Writer.t
    ; mutable listening : bool
    ; socket            : ([ `Passive ], Socket.Address.Inet.t) Socket.t
    ; auth              : (Unix.Inet_addr.t
                           -> int
                           -> Client_id.t
                           -> [ `Allow
                              | `Deny of string option
                              ] Deferred.t)
    ; buffer_age_limit  : [ `At_most of Time.Span.t | `Unlimited ] option
    ; server_port       : int
    }

  let try_with = Monitor.try_with
  let ignore_errors f = don't_wait_for (try_with f >>| ignore)

  exception Exception_while_reading of exn [@@deriving sexp]

  exception Eof_from_client [@@deriving sexp]
  exception Pipe_closed [@@deriving sexp]

  let handle_client t ~close ~transport ~id ~stop =
    let rec loop () =
      let res =
        choose
          [ choice stop (fun () -> `Stop);
            choice (try_with (fun () -> Transport.read transport)) (fun x -> `Read x) ;
          ]
      in
      res
      >>> function
      | `Stop -> ()
      | `Read x ->
        match Deferred.peek stop with
        | Some () -> ()
        | None ->
          match x with
          | Error e -> close (Exception_while_reading e)
          | Ok x ->
            match x with
            | `Eof ->
              (* This may be a client-initiated disconnect, so it is not necessarily an
                 error. *)
              close Eof_from_client
            | `Ok a ->
              if Pipe.is_closed t.result_writer
              then close Pipe_closed
              else Pipe.write t.result_writer (Data (id, a)) >>> loop
    in
    loop ()

  exception Connection_closed of exn [@@deriving sexp]

  exception Exception_in_writer of exn [@@deriving sexp]

  let listener t =
    let rec loop () =
      try_with (fun () -> Socket.accept t.socket)
      >>> function
      | Error exn ->
        Monitor.send_exn (Monitor.current ()) exn;
        Clock.after (sec 0.5) >>> loop
      | Ok `Socket_closed ->
        (* stop looping when socket is closed *)
        ()
      | Ok (`Ok (sock, `Inet (addr, port))) ->
        (* Go ahead and accept more connections. *)
        loop ();
        if t.verbose then
          eprintf "accepted connection from %s:%d\n"
            (Unix.Inet_addr.to_string addr) port;
        let fd = Socket.fd sock in
        let id = Client_id.create () in
        t.auth addr port id >>> function
        | `Deny reason ->
          let msg =
            sprintf "denied access to %s%s"
              (Unix.Inet_addr.to_string addr)
              (match reason with
               | None -> " no reason given"
               | Some r -> sprintf " reason %s" r)
          in
          if t.verbose then eprintf "%s\n%!" msg;
          (if not (Pipe.is_closed t.result_writer)
           then Pipe.write t.result_writer (Denied_access msg)
           else Deferred.unit)
          >>> fun () ->
          ignore_errors (fun () -> Unix.close fd)
        | `Allow ->
          if t.verbose then eprintf "access allowed\n";
          let writer =
            Writer.create ?buffer_age_limit:t.buffer_age_limit fd
          in
          let reader = Reader.create fd in
          Transport.create reader writer >>> fun transport ->
          let closed = Ivar.create () in
          let close =
            let disconnect_reason = ref None in
            let close =
              lazy (
                match !disconnect_reason with
                | None -> assert false
                | Some disconnect_reason ->
                  ignore_errors (fun () -> Transport.close transport);
                  don't_wait_for
                    (if not (Pipe.is_closed t.result_writer)
                     then Pipe.write t.result_writer
                            (Disconnect (id, Exn.sexp_of_t disconnect_reason))
                     else Deferred.unit);
                  Hashtbl.remove t.clients id;
                  Ivar.fill closed ();
                  if t.log_disconnects then
                    eprintf "%s\n"
                      (Exn.to_string (Connection_closed disconnect_reason)))
            in
            (fun e -> disconnect_reason := Some e; Lazy.force close)
          in
          Stream.iter (Monitor.detach_and_get_error_stream (Writer.monitor writer)) ~f:(fun e ->
            close (Exception_in_writer e));
          assert (not (Hashtbl.mem t.clients id));
          Hashtbl.set t.clients ~key:id ~data:{ close; transport; addr; port };
          if Pipe.is_closed t.result_writer
          then close Pipe_closed
          else begin
            Pipe.write t.result_writer (Connect id)
            >>> fun () ->
            handle_client t ~close ~transport ~id
              ~stop:(Ivar.read closed)
          end
    in
    loop ()
  ;;

  let maybe_start_listener t =
    if not t.listening then begin
      t.listening <- true;
      listener t
    end
  ;;

  let create ?backlog
        ?(verbose = false)
        ?(log_disconnects = true)
        ?buffer_age_limit ~port ~auth () =
    let s = Socket.create Socket.Type.tcp in
    Monitor.try_with (fun () ->
      Socket.bind s (Socket.Address.Inet.create_bind_any ~port))
    >>= function
    | Error e ->
      Unix.close (Socket.fd s) >>| fun () -> raise e
    | Ok s ->
      let server_port = Socket.Address.Inet.port (Socket.getsockname s) in
      let s           = Socket.listen s ?backlog in
      let result_reader, result_writer = Pipe.create () in
      let t =
        { verbose
        ; log_disconnects
        ; clients          = Client_id.Table.create ()
        ; result_reader
        ; result_writer
        ; listening        = false
        ; socket           = s
        ; auth
        ; buffer_age_limit
        ; server_port
        }
      in
      return t
  ;;

  let port t = t.server_port

  let client_addr_port t id =
    Option.map (Hashtbl.find t.clients id) ~f:(fun cl ->
      (cl.addr, cl.port))

  let listen t =
    maybe_start_listener t;
    t.result_reader

  let listen_ignore_errors t =
    Pipe.filter_map (listen t) ~f:(function
      | Connect _
      | Disconnect _
      | Denied_access _ -> None
      | Data (id, a) -> Some (id, a))

  let send_ignore_errors t id m =
    match Hashtbl.find t.clients id with
    | None -> ()
    | Some client -> Transport.write client.transport m

  let has_client_id t id =
    Hashtbl.mem t.clients id

  exception Missing_client_id of Client_id.t [@@deriving sexp]

  exception Exception_while_writing of exn [@@deriving sexp]

  let send t id m =
    match Hashtbl.find t.clients id with
    | None -> return (`Drop (Missing_client_id id))
    | Some client ->
      let transport = client.transport in
      let error exn =
        client.close (Exception_while_writing exn);
        `Drop exn
      in
      (* We call [Transport.write] synchronously, so that messages sent via successive
         calls to [send] are sent in sequence. *)
      match Result.try_with (fun () -> Transport.write transport m) with
      | Error exn -> return (error exn)
      | Ok () ->
        try_with (fun () -> Transport.flushed_time transport >>| fun tm -> `Sent tm)
        >>| function
        | Ok res -> res
        | Error exn -> error exn
  ;;

  let send_to_all t m =
    Hashtbl.iter t.clients
      ~f:(fun client -> Transport.write client.transport m)
  ;;

  exception Closed_by_server [@@deriving sexp]

  let close t id =
    match Hashtbl.find t.clients id with
    | None -> ()
    | Some client -> client.close Closed_by_server
  ;;

  let close_server t =
    List.iter (Hashtbl.data t.clients) ~f:(fun client -> client.close Closed_by_server);
    Unix.close (Socket.fd t.socket)
  ;;

  let flushed_time t id =
    match Hashtbl.find t.clients id with
    | None -> `Client_not_found
    | Some client -> `Flushed (Transport.flushed_time client.transport)
  ;;
end

module Simple
    (Client_to_server : Binable_t)
    (Server_to_client : Binable_t) () =
  Make (struct
    module Client_message = Client_to_server
    module Server_message = Server_to_client

    module Transport = struct
      type t = Reader.t * Writer.t

      let create r w = return (r, w)
      let read (r, _) = Reader.read_bin_prot r Client_message.bin_reader_t
      let write (_, w) m = Writer.write_bin_prot w Server_message.bin_writer_t m
      let flushed_time (_, w) = Writer.flushed_time w
      let close (_, w) = Writer.close w
    end
  end) ()
