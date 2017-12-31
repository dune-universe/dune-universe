
open Core
open Import

include Versioned_typed_tcp_intf

exception Bigsubstring_allocator_got_invalid_requested_size of int [@@deriving sexp]

let bigsubstring_allocator ?(initial_size = 512) () =
  let buf = ref (Bigstring.create initial_size) in
  fun requested_size ->
    if requested_size < 1 then
      raise (Bigsubstring_allocator_got_invalid_requested_size requested_size);
    if requested_size > Bigstring.length !buf then
      buf := (Bigstring.create
                (Int.max requested_size (2 * Bigstring.length !buf)));
    Bigsubstring.create !buf ~pos:0 ~len:requested_size
;;

let protocol_version : [ `Prod | `Test ] ref = ref `Test

module Dont_care_about_mode = struct
  type t = Dont_care_about_mode [@@deriving bin_io, sexp]


  let current () = Dont_care_about_mode
  let (=) (a : t) b = a = b
end

(* helper types to make signatures clearer *)
type 'a wo_my_name =
  < send : 'send;
    recv : 'recv;
    remote_name : 'remote_name >
  constraint 'a =
    < send : 'send;
    recv : 'recv;
    my_name     : 'my_name;
    remote_name : 'remote_name >

type 'a flipped =
  < send : 'recv;
    recv : 'send;
    my_name     : 'remote_name;
    remote_name : 'my_name;
  >
  constraint 'a =
    < send : 'send;
    recv : 'recv;
    my_name : 'my_name;
    remote_name : 'remote_name >

module Make (Z : Arg) = struct
  include Z

  module Constants = struct
    let negotiate_timeout = sec 10.

    let wait_after_connect_failure = sec 4.
  end

  open Constants

  type 'a logfun =
    [ `Recv of 'recv | `Send of 'send ]
    -> 'remote_name
    -> time_sent_received:Time.t
    -> unit
    constraint 'a = < send : 'send; recv : 'recv; remote_name : 'remote_name >

  module Hello = struct
    type t =
      { name         : string
      ; mode         : Mode.t
      ; send_version : Version.t
      ; recv_version : Version.t
      ; credentials  : string
      }
    [@@deriving bin_io, sexp]

    let create ~name ~send_version ~recv_version ~credentials =
      { name
      ; mode         = Mode.current ()
      ; send_version
      ; recv_version
      ; credentials
      }
  end

  (* After negotiation messages on the wire are composed of a header,
     and then a message body *)
  module Message_header = struct
    type t =
      { time_stamp  : Time.t
      ; body_length : int
      }
    [@@deriving bin_io, sexp]
  end

  module Connection = struct
    type 'a t =
      { writer        : Writer.t
      ; reader        : Reader.t
      ; marshal_fun   : 'send marshal_fun
      ; unmarshal_fun : 'recv unmarshal_fun
      ; send_version  : Version.t
      ; my_name       : 'my_name
      ; remote_name   : 'remote_name
      ; kill          : unit -> unit
      }
      constraint 'a =
        < send        : 'send;
        recv        : 'recv;
        my_name     : 'my_name;
        remote_name : 'remote_name >

                      let kill t = t.kill ()
  end

  let try_with = Monitor.try_with

  let ignore_errors f = don't_wait_for (try_with f >>| ignore)

  let try_with_timeout span f =
    choose
      [ choice (Clock.after span) (fun () -> `Timeout);
        choice (try_with f) (function
          | Ok x -> `Ok x
          | Error x -> `Error x)
      ]
  ;;

  module Write_bin_prot_error = struct
    type t =
      { name      : string
      ; arg       : Sexp.t
      ; exn       : exn
      ; backtrace : Backtrace.t;
      }
    [@@deriving sexp_of]

    exception E of t [@@deriving sexp]
  end

  let wrap_write_bin_prot ~sexp ~tc ~writer ~name m =
    try Writer.write_bin_prot writer tc m
    with exn ->
      raise (Write_bin_prot_error.E { name
                                    ; arg       = sexp m
                                    ; exn
                                    ; backtrace = Backtrace.Exn.most_recent ()
                                    })
  ;;

  let send_raw ~writer ~hdr ~msg =
    wrap_write_bin_prot
      ~sexp:Message_header.sexp_of_t ~tc:Message_header.bin_t.writer ~writer
      ~name:"send" hdr;
    Writer.write_bigsubstring writer msg
  ;;

  let send_no_flush =
    let maybe_log ~logfun ~name ~now d =
      match logfun with
      | None -> ()
      | Some f -> f (`Send d) name ~time_sent_received:now
    in
    fun ~logfun ~name ~now (con : _ Connection.t) d ->
      match con.marshal_fun d with
      | None -> `Not_sent
      | Some msg ->
        let now = now () in
        let hdr =
          { Message_header. time_stamp = now; body_length = Bigsubstring.length msg }
        in
        send_raw ~writer:con.writer ~hdr ~msg;
        maybe_log ~logfun ~name ~now d;
        `Sent
  ;;

  let send ~logfun ~name ~now con d =
    match send_no_flush ~logfun:None ~name ~now con d with
    | `Sent ->
      Writer.flushed_time con.writer >>| fun tm ->
      begin match logfun with
      | None -> ()
      | Some f -> f (`Send d) name ~time_sent_received:(now ())
      end;
      `Sent tm
    | `Not_sent -> return `Dropped
  ;;

  let negotiate (type r) (type s)
        ~reader ~writer ~my_name ~credentials
        ~recv ~send ?received_hello () =
    let module Recv = (val recv : Datum with type t = r) in
    let module Send = (val send : Datum with type t = s) in
    let (recv_version, send_version) =
      match !protocol_version with
      | `Prod -> (Recv.prod_version, Send.prod_version)
      | `Test -> (Recv.test_version, Send.test_version)
    in
    let my_hello =
      Hello.create
        ~name:my_name
        ~send_version
        ~recv_version
        ~credentials
    in
    wrap_write_bin_prot ~sexp:Hello.sexp_of_t ~tc:Hello.bin_writer_t
      ~writer ~name:"negotiate" my_hello;
    begin match received_hello with
    | Some h -> return (`Ok h)
    | None -> Reader.read_bin_prot reader Hello.bin_reader_t
    end
    >>| function
    | `Eof -> `Eof
    | `Ok received_hello ->
      if not Mode.(my_hello.mode = received_hello.mode)
      then `Wrong_mode received_hello.name
      else
        begin
          let recv_version = Version.min my_hello.recv_version received_hello.send_version in
          let send_version = Version.min my_hello.send_version received_hello.recv_version in
          match Recv.lookup_unmarshal_fun recv_version with
          | Error _ -> `Version_error
          | Ok unmarshal_fun ->
            match Send.lookup_marshal_fun send_version with
            | Error _ -> `Version_error
            | Ok marshal_fun ->
              `Ok (received_hello, send_version, marshal_fun, unmarshal_fun)
        end
  ;;

  exception Eof [@@deriving sexp]
  exception Unconsumed_data of string [@@deriving sexp]

  let dummy_bigsubstring = Bigsubstring.of_string ""

  let handle_incoming
        ~logfun ~ip ~(con : _ Connection.t) ~extend_data_needs_raw
        (* functions to extend the tail (with various messages) *)
        ~extend_disconnect
        ~extend_parse_error
        ~extend_data
    =
    let remote_name = con.remote_name in
    Writer.set_raise_when_consumer_leaves con.writer false;
    upon (Writer.consumer_left con.writer) con.kill;
    (* Benign errors like EPIPE and ECONNRESET will not be raised, so we are left with
       only serious errors ("man 2 write" lists EBADF, EFAULT, EFBIG, EINVAL, EIO, ENOSPC,
       all of which point to either a bug in async or to a pretty bad state of the
       system).  We close the connection and propagate the error up. *)
    Stream.iter (Monitor.detach_and_get_error_stream (Writer.monitor con.writer)) ~f:(fun e ->
      con.kill ();
      (* As opposed to [raise], this will continue propagating subsequent exceptions. *)
      (* This can't lead to an infinite loop because con.writer is not exposed *)
      Monitor.send_exn (Monitor.current ()) e);
    let extend ~time_sent ~time_received data raw =
      begin match logfun with
      | None -> ()
      | Some f -> f (`Recv data) remote_name ~time_sent_received:time_received
      end;
      extend_data
        { Read_result.
          from          = remote_name
        ; data
        ; ip
        ; time_received
        ; time_sent
        }
        raw
    in
    let len_len = 8 in
    let pos_ref = ref 0 in
    let rec handle_chunk buf ~consumed ~pos ~len =
      if len = 0
      then return `Continue
      else if len_len > len
      then return (`Consumed (consumed, `Need len_len))
      else begin
        pos_ref := pos;
        (* header has two fields, [time_stamp] which is 8 bytes over bin_io, and
           [body_length] which is variable length encoded int that can in theory be 1 to 8
           bytes over bin_io, but in practice it doesn't take more than 3 bytes *)
        let hdr_len = Bin_prot.Read.bin_read_int_64bit buf ~pos_ref in
        if len_len + hdr_len > len
        then return (`Consumed (consumed, `Need (len_len + hdr_len)))
        else begin
          let hdr =
            try
              Ok (Message_header.bin_reader_t.read buf ~pos_ref)
            with
              exn -> Error exn
          in
          match hdr with
          | Error exn ->
            return (`Stop exn)
          | Ok hdr ->
            let body_len = hdr.body_length in
            let msg_len = len_len + hdr_len + body_len in
            if msg_len > len
            then return (`Consumed (consumed, `Need msg_len))
            else begin
              let body = Bigsubstring.create buf ~pos:(!pos_ref) ~len:body_len in
              begin match try Ok (con.unmarshal_fun body) with ex -> Error ex with
              | Error exn ->
                extend_parse_error remote_name (Exn.to_string exn);
              | Ok None -> ()
              | Ok (Some msg) ->
                let time_received = Reader.last_read_time con.reader in
                let time_sent = hdr.time_stamp in
                let raw_msg =
                  if extend_data_needs_raw
                  then Bigsubstring.create buf ~pos ~len:msg_len
                  else
                    (* a performance hack: this isn't really used downstream *)
                    dummy_bigsubstring
                in
                extend ~time_received ~time_sent msg raw_msg
              end;
              handle_chunk
                buf
                ~consumed:(consumed + msg_len)
                ~pos:(pos + msg_len)
                ~len:(len - msg_len)
            end
        end
      end
    in
    Reader.read_one_chunk_at_a_time con.reader
      ~handle_chunk:(handle_chunk ~consumed:0)
    >>| (fun result ->
      con.kill ();
      let exn =
        match result with
        | `Eof -> Eof
        | `Stopped exn -> exn
        | `Eof_with_unconsumed_data data -> (Unconsumed_data data)
      in
      extend_disconnect remote_name exn)
  ;;

  module Server = struct

    type dir = < send : To_client_msg.t;
                 recv : To_server_msg.t;
                 my_name : Server_name.t;
                 remote_name : Client_name.t >

    module Connection = struct
      type t = dir Connection.t

      let kill = Connection.kill
    end

    type nonrec logfun = dir wo_my_name logfun

    module Connections : sig
      type t
      val create : unit -> t
      val mem : t -> Client_name.t -> bool
      val find : t -> Client_name.t -> Connection.t option
      val add : t -> name:Client_name.t -> conn:Connection.t -> unit
      val remove : t -> Client_name.t -> unit

      val fold
        : t
        -> init:'a
        -> f:(name:Client_name.t -> conn:Connection.t -> 'a -> 'a)
        -> 'a

      val kill_all
        : t
        -> unit

      val send_to_all
        :  t
        -> logfun:logfun option
        -> now:(unit -> Time.t)
        -> To_client_msg.t
        -> [ `Sent (** sent successfuly to all clients *)
           | `Dropped (** not sent successfully to any client *)
           | `Partial_success (** sent to some clients *)
           ] Deferred.t

      val send_to_all_ignore_errors : t
        -> logfun:logfun option
        -> now:(unit -> Time.t)
        -> To_client_msg.t
        -> unit

      val send_to_some : t
        -> logfun:logfun option
        -> now:(unit -> Time.t)
        -> To_client_msg.t
        -> Client_name.t list
        -> [ `Sent (** sent successfuly to all clients *)
           | `Dropped (** not sent successfully to any client *)
           | `Partial_success (** sent to some clients *)] Deferred.t

      val send_to_some_ignore_errors : t
        -> logfun:logfun option
        -> now:(unit -> Time.t)
        -> To_client_msg.t
        -> Client_name.t list
        -> unit
    end = struct

      module C = Connection

      type t =
        { by_name         : (C.t Bag.t * C.t Bag.Elt.t) Client_name.Table.t
        ; by_send_version : (C.t Bag.t * To_client_msg.t marshal_fun) Version.Table.t
        }

      let create () =
        { by_name = Client_name.Table.create ()
        ; by_send_version =
            Version.Table.create
              ~size:(Version.to_int To_client_msg.test_version) ();
        }
      ;;

      let fold t ~init ~f =
        Hashtbl.fold t.by_name ~init ~f:(fun ~key ~data:(_, bag_elt) acc ->
          f ~name:key ~conn:(Bag.Elt.value bag_elt) acc)
      ;;

      let kill_all t =
        (* We don't use [iter] here as [kill] mutates the structure we are iterating
           over. *)
        let connections =
          fold t ~init:[] ~f:(fun ~name:_ ~conn acc -> conn :: acc)
        in
        List.iter connections ~f:Connection.kill
      ;;

      let mem t name = Hashtbl.mem t.by_name name

      let add t ~name ~(conn : Connection.t) =
        let bag, _marshal_fun =
          Hashtbl.find_or_add t.by_send_version conn.send_version
            ~default:(fun () -> Bag.create (), conn.marshal_fun)
        in
        let bag_elt = Bag.add bag conn in
        Hashtbl.set t.by_name ~key:name ~data:(bag, bag_elt);
      ;;

      let remove t name =
        match Hashtbl.find t.by_name name with
        | None -> ()
        | Some (bag, bag_elt) ->
          Bag.remove bag bag_elt;
          Hashtbl.remove t.by_name name;
      ;;

      let find t name =
        match Hashtbl.find t.by_name name with
        | None -> None
        | Some (_, bag_elt) -> Some (Bag.Elt.value bag_elt)
      ;;

      let maybe_log ~logfun ~name ~now d =
        match logfun with
        | None -> ()
        | Some f -> f (`Send d) name ~time_sent_received:now
      ;;

      let schedule_bigstring_threshold = 64 * 1024 (* half the size of writer's buffer *)

      let send_to_some'' d ~logfun ~now by_version is_bag_empty iter_bag =
        let now = now () in
        let res =
          Hashtbl.fold by_version ~init:`Init
            ~f:(fun ~key:_ ~data:(bag, marshal_fun) acc ->
              if is_bag_empty bag
              then acc
              else begin
                let res =
                  match marshal_fun d with
                  | None -> `Dropped
                  | Some msg ->
                    let body_length = Bigsubstring.length msg in
                    let hdr = { Message_header. time_stamp = now; body_length } in
                    let send =
                      if body_length > schedule_bigstring_threshold
                      then begin
                        let to_schedule = Bigstring.create body_length in
                        Bigsubstring.blit_to_bigstring msg ~dst:to_schedule ~dst_pos:0;
                        fun (conn : C.t) ->
                          wrap_write_bin_prot
                            ~sexp:Message_header.sexp_of_t
                            ~tc:Message_header.bin_t.Bin_prot.Type_class.writer
                            ~writer:conn.writer
                            ~name:"send" hdr;
                          Writer.schedule_bigstring conn.writer to_schedule
                      end
                      else begin
                        fun conn ->
                          send_raw ~writer:conn.writer ~hdr ~msg;
                      end
                    in
                    iter_bag bag ~f:(fun conn ->
                      send conn;
                      maybe_log ~logfun ~now ~name:conn.remote_name d);
                    `Sent
                in
                match acc, res with
                | `Partial_success, _
                | `Sent           , `Dropped
                | `Dropped        , `Sent
                  -> `Partial_success
                | `Sent           , `Sent
                  -> `Sent
                | `Dropped        , `Dropped
                  -> `Dropped
                | `Init           , _
                  -> res
              end)
        in
        match res with
        | `Init -> `Sent
        | `Partial_success | `Dropped | `Sent as x -> x
      ;;

      let send_to_all' t d ~logfun ~now =
        send_to_some'' d ~logfun ~now t.by_send_version Bag.is_empty Bag.iter
      ;;

      let send_to_all t ~logfun ~now d =
        let res = send_to_all' t d ~logfun ~now in
        Deferred.all_unit (fold t ~init:[] ~f:(fun ~name:_ ~conn acc ->
          Writer.flushed conn.writer :: acc))
        >>| fun () -> res
      ;;

      let send_to_all_ignore_errors t ~logfun ~now d =
        ignore (send_to_all' t d ~logfun ~now : [`Partial_success | `Dropped | `Sent])
      ;;

      let send_to_some' t d ~logfun ~now names =
        let tbl =
          Version.Table.create ~size:(Version.to_int To_client_msg.test_version) ()
        in
        let all_names_found =
          List.fold names ~init:true ~f:(fun acc name ->
            let conn = find t name in
            match conn with
            | None -> false
            | Some conn ->
              let version = conn.send_version in
              let conns =
                match Hashtbl.find tbl version with
                | Some (conns, _marshal_fun) -> conns
                | None -> []
              in
              Hashtbl.set tbl ~key:version ~data:(conn::conns, conn.marshal_fun);
              acc)
        in
        if Hashtbl.is_empty tbl
        then `Dropped
        else
          let res = send_to_some'' d ~logfun ~now tbl List.is_empty List.iter in
          if all_names_found
          then res
          else
            match res with
            | `Sent | `Partial_success -> `Partial_success
            | `Dropped -> `Dropped
      ;;

      let send_to_some t ~logfun ~now d names =
        let res = send_to_some' t d ~logfun ~now names in
        Deferred.all_unit (fold t ~init:[] ~f:(fun ~name:_ ~conn acc ->
          Writer.flushed conn.writer :: acc))
        >>| fun () -> res
      ;;

      let send_to_some_ignore_errors t ~logfun ~now d names =
        ignore (send_to_some' t d ~logfun ~now names : [`Partial_success | `Dropped | `Sent])
      ;;
    end

    type t =
      { tail                       : (Client_name.t, To_server_msg.t) Server_msg.t Tail.t
      ; logfun                     : logfun option
      ; connections                : Connections.t
      ; warn_free_connections_pct  : float
      ; max_clients                : int
      ; is_client_ip_authorized    : string -> bool
      ; my_name                    : Server_name.t
      ; enforce_unique_remote_name : bool
      ; now                        : unit -> Time.t
      ; mutable num_accepts        : Int63.t
      ; credentials                : string
      ; tcp_server : Tcp.Server.inet
      ; handler : (Socket.Address.Inet.t -> Reader.t -> Writer.t -> unit Deferred.t) Set_once.t
      }

    let invariant t =
      let num_connections = Tcp.Server.num_connections t.tcp_server in
      let free_connections = t.max_clients - num_connections in
      assert (free_connections >= 0);
      assert (free_connections <= t.max_clients);
    ;;

    let flushed t ~cutoff =
      let flushes =
        Connections.fold t.connections ~init:[]
          ~f:(fun ~name:client ~conn acc ->
            (choose [ choice (Writer.flushed conn.writer)
                        (fun _ -> `Flushed client);
                      choice cutoff (fun () -> `Not_flushed client);
                    ]) :: acc)
      in
      Deferred.all flushes >>| fun results ->
      let (flushed, not_flushed) =
        List.partition_map results
          ~f:(function `Flushed c -> `Fst c | `Not_flushed c -> `Snd c)
      in
      `Flushed flushed, `Not_flushed not_flushed
    ;;

    let shutdown t = Tcp.Server.close t.tcp_server

    let shutdown_and_disconnect_clients t =
      shutdown t
      >>| fun () ->
      Connections.kill_all t.connections
    ;;

    let send t name d =
      match Connections.find t.connections name with
      | None -> return `Dropped
      | Some c -> send ~logfun:t.logfun ~name ~now:t.now c d
    ;;

    let send_ignore_errors t name d =
      match Connections.find t.connections name with
      | None -> ()
      | Some c -> ignore (send_no_flush ~logfun:t.logfun ~name ~now:t.now c d)
    ;;

    let send_to_all t d =
      Connections.send_to_all t.connections d ~now:t.now ~logfun:t.logfun
    ;;

    let send_to_all_ignore_errors t d =
      Connections.send_to_all_ignore_errors t.connections d ~now:t.now ~logfun:t.logfun
    ;;

    let send_to_some t d names =
      Connections.send_to_some t.connections d ~now:t.now ~logfun:t.logfun names
    ;;

    let send_to_some_ignore_errors t d names =
      Connections.send_to_some_ignore_errors t.connections d ~now:t.now ~logfun:t.logfun names
    ;;

    let close t name =
      Option.iter (Connections.find t.connections name) ~f:Connection.kill
    ;;

    let maybe_accept_client t addr reader writer ?received_hello () =
      let control_error (e : _ Server_msg.Control.t) = return (Error e) in
      let protocol_error msg =
        let s = sprintf "Error while negotiating with [%s]: %s"
                  (Socket.Address.Inet.to_string addr)
                  msg
        in
        control_error (Protocol_error s)
      in
      let ip = Unix.Inet_addr.to_string (Socket.Address.Inet.addr addr) in
      if not (t.is_client_ip_authorized ip)
      then begin
        control_error (Unauthorized ip)
      end else begin
        try_with_timeout negotiate_timeout (fun () ->
          negotiate
            ~my_name:(Server_name.to_string t.my_name)
            ~credentials:t.credentials
            ~reader ~writer
            ~send:(module To_client_msg)
            ~recv:(module To_server_msg)
            ?received_hello ()
        )
        >>= function
        | `Error e -> protocol_error (Exn.to_string e)
        | `Timeout -> protocol_error "Timeout"
        | `Ok `Eof -> protocol_error "Connection dropped"
        | `Ok `Version_error -> protocol_error "Couldn't negotiate version"
        | `Ok (`Wrong_mode remote_name) ->
          control_error
            (Wrong_mode
               (Client_name.of_string remote_name))
        | `Ok (`Ok (h, send_version, marshal_fun, unmarshal_fun)) ->
          let name =
            if t.enforce_unique_remote_name
            then h.name
            else sprintf "%s:%s:%s"
                   h.name
                   (Socket.Address.Inet.to_string addr)
                   (Int63.to_string t.num_accepts)
          in
          match Result.try_with (fun () -> Client_name.of_string name) with
          | Error exn ->
            protocol_error
              (sprintf "error constructing name: %s, error: %s"
                 name (Exn.to_string exn))
          | Ok client_name ->
            if Connections.mem t.connections client_name
            then control_error (Duplicate client_name)
            else begin
              let close =
                lazy
                  (ignore_errors (fun () ->
                     Writer.close writer ~force_close:(Clock.after (sec 5.)));
                   Connections.remove t.connections client_name)
              in
              let kill = (fun () -> Lazy.force close) in
              let (conn : Connection.t) =
                { writer
                ; reader
                ; unmarshal_fun
                ; marshal_fun
                ; send_version
                ; my_name       = t.my_name
                ; remote_name   = client_name
                ; kill
                }
              in
              Connections.add t.connections ~name:client_name ~conn;
              Tail.extend t.tail
                (Control (Connect (conn.remote_name, `credentials h.credentials)));
              return (Ok (conn, ip))
            end
      end
    ;;

    let handle_incoming t (conn : Connection.t) ip =
      handle_incoming
        ~logfun:t.logfun
        ~ip ~con:conn
        ~extend_data_needs_raw:false
        ~extend_disconnect:(fun n e ->
          Tail.extend t.tail (Control (Disconnect (n, Exn.sexp_of_t e))))
        ~extend_parse_error:(fun n e ->
          Tail.extend t.tail (Control (Parse_error (n, e))))
        ~extend_data:(fun x _ -> Tail.extend t.tail (Data x))
    ;;

    let listen' t ~handler =
      begin match Set_once.get t.handler with
      | Some _ -> ()
      | None ->
        let wrapped_handler =
          let control e = Tail.extend t.tail (Control e) in
          let warn_thres =
            Int.max 1 (Float.iround_exn ~dir:`Zero
                         (float t.max_clients *. t.warn_free_connections_pct))
          in
          fun addr reader writer ->
            t.num_accepts <- Int63.succ t.num_accepts;
            let num_connections = Tcp.Server.num_connections t.tcp_server in
            let free_connections = t.max_clients - num_connections in
            assert (free_connections >= 0);
            begin if free_connections = 0 then
                control (Too_many_clients "zero free connections")
              else if free_connections <= warn_thres then
                control (Almost_full free_connections);
            end;
            handler addr reader writer
        in
        Set_once.set_exn t.handler [%here] wrapped_handler;
      end;
      Tail.collect t.tail
    ;;

    let listen t =
      listen' t ~handler:(fun addr reader writer ->
        maybe_accept_client t addr reader writer ()
        >>= function
        | Error e ->
          Tail.extend t.tail (Control e);
          (* This determines the handler in [Tcp.Server.create], which causes the
             descriptors to be closed, as explained in [tcp.mli]. *)
          Deferred.unit
        | Ok (conn, ip) ->
          Monitor.try_with (fun () -> handle_incoming t conn ip)
          >>| fun res ->
          Connection.kill conn; (* kill connection just in case *)
          match res with
          | Ok () -> ()
          | Error exn ->
            Tail.extend t.tail (Control (Disconnect (conn.remote_name, Exn.sexp_of_t exn)));
      )
    ;;

    let listen_ignore_errors ?(stop = Deferred.never ()) t =
      let s = Stream.take_until (listen t) stop in
      Stream.filter_map_deprecated s ~f:(function
        | Control _ -> None
        | Data x -> Some x.data)
    ;;

    let create'
          ?logfun
          ?(now = Scheduler.cycle_start)
          ?(enforce_unique_remote_name = true)
          ?(is_client_ip_authorized = fun _ -> true)
          ?(warn_when_free_connections_lte_pct = 0.05)
          ?(max_clients = 500)
          ?(credentials = "")
          ~listen_port
          my_name =
      if max_clients > 10_000 || max_clients < 1 then
        raise (Invalid_argument "max_clients must be between 1 and 10,000");
      let handler = Set_once.create () in
      Tcp.Server.create
        ~on_handler_error:`Raise
        ~max_connections:max_clients
        ~backlog:(min 1_000 max_clients)
        (Tcp.Where_to_listen.of_port listen_port)
        (fun addr reader writer ->
           match Set_once.get handler with
           | None ->
             (* This determines the handler in [Tcp.Server.create], which causes the
                descriptors to be closed, as explained in [tcp.mli]. *)
             Deferred.unit (* refuse connections until [listen] is called *)
           | Some handler ->
             handler addr reader writer)
      >>| fun tcp_server ->
      let t =
        { tail = Tail.create ()
        ; handler
        ; logfun
        ; connections = Connections.create ()
        ; is_client_ip_authorized
        ; my_name
        ; enforce_unique_remote_name
        ; warn_free_connections_pct = warn_when_free_connections_lte_pct
        ; max_clients
        ; now
        ; credentials
        ; num_accepts = Int63.zero
        ; tcp_server
        }
      in
      t
    ;;

    let create = create' ?credentials:None
    ;;

    let port t = Tcp.Server.listening_on t.tcp_server

    let client_send_version t name =
      match Connections.find t.connections name with
      | None -> None
      | Some conn -> Some conn.send_version
    ;;

    let client_is_connected t name = Connections.mem t.connections name
  end

  module Client = struct

    type dir = < send : To_server_msg.t;
                 recv : To_client_msg.t;
                 my_name : Client_name.t;
                 remote_name : Server_name.t >

    type nonrec logfun = dir wo_my_name logfun

    module Connection = struct
      type t = dir Connection.t

      let kill = Connection.kill
    end

    type t =
      { remote_ip                  : Unix.Inet_addr.t
      ; remote_port                : int
      ; expected_remote_name       : Server_name.t
      ; check_remote_name          : bool (* check whether the server's name
                                             matches the expected remote name*)
      ; logfun                     : logfun option
      ; my_name                    : Client_name.t
      ; messages                   : (Server_name.t, To_client_msg.t) Client_msg.t Tail.t
      ; queue                      : (To_server_msg.t
                                      * [ `Sent of Time.t | `Dropped] Ivar.t
                                     ) Queue.t
      ; mutable con : [ `Disconnected of Time.t (* last disconnect time *)
                      | `Connected of Connection.t
                      | `Connecting of unit Ivar.t
                      ]
      ; mutable connect_complete   : unit Ivar.t
      ; now                        : unit -> Time.t
      (* last connection error. None if it has succeeded*)
      ; mutable last_connect_error : Error.t option
      ; credentials                : string
      }

    (* [connect_internal t] makes a single try to connect to the server, negotiate
       connection and then run the handler on the result.

       Tcp.with_connection will throw an exception if it can't establish a connection.
       We need to distinguish between that exception and handler's exceptions, hence the
       result is a variant [ `Connect_error _ | `Handler_error _ | `Ok _ ]  *)
    let connect_internal t ~handler =
      let stop_connecting =
        match t.con with
        | `Connected _ | `Disconnected _ -> assert false
        | `Connecting stop_connecting -> stop_connecting
      in
      Monitor.try_with (fun () ->
        Tcp.with_connection
          ~interrupt:(Ivar.read stop_connecting)
          (Tcp.Where_to_connect.of_inet_address
             (Socket.Address.Inet.create t.remote_ip ~port:t.remote_port))
          (fun _socket reader writer ->
             let my_name = Client_name.to_string t.my_name in
             try_with_timeout negotiate_timeout (fun () ->
               negotiate ~reader ~writer
                 ~send:(module To_server_msg)
                 ~recv:(module To_client_msg)
                 ~credentials:t.credentials
                 ~my_name ()
             )
             >>| (function
               | `Error exn -> Or_error.of_exn exn
               | `Timeout ->
                 Or_error.error_string "Timeout while negotiating"
               | `Ok `Eof ->
                 Or_error.error_string "Disconnect while negotiating"
               | `Ok (`Wrong_mode server_name) ->
                 Or_error.error "Wrong mode" server_name String.sexp_of_t
               | `Ok `Version_error ->
                 Or_error.error_string "cannot negotiate a common version"
               | `Ok (`Ok (h, send_version, marshal_fun, unmarshal_fun)) ->
                 if Ivar.is_full stop_connecting then
                   Or_error.error_string "stop connecting requested"
                 else begin
                   let expected_remote_name =
                     Server_name.to_string t.expected_remote_name
                   in
                   if t.check_remote_name
                   && h.name <> expected_remote_name
                   then
                     Or_error.error "Hello name is not expected remote name"
                       (h.name, expected_remote_name)
                       [%sexp_of: string * string]
                   else begin
                     let server_name = Server_name.of_string h.name in
                     let close =
                       lazy
                         (ignore_errors (fun () ->
                            Writer.close writer ~force_close:(Clock.after (sec 5.))))
                     in
                     let kill = (fun () -> Lazy.force close) in
                     let (conn : Connection.t) =
                       { writer
                       ; reader
                       ; marshal_fun
                       ; unmarshal_fun
                       ; send_version
                       ; remote_name   = server_name
                       ; my_name       = t.my_name
                       ; kill
                       }
                     in
                     Ok (conn, h.credentials)
                   end
                 end)
             >>= fun res ->
             Monitor.try_with (fun () -> handler res)
             >>| function
             | Error e -> `Handler_error e
             | Ok x    -> `Ok x))
      >>| function
      | Error e -> `Connect_error e
      | Ok (`Handler_error _ as handler_error) -> handler_error
      | Ok (`Ok _ as ok) -> ok

    ;;

    let handle_incoming t =
      match t.con with
      | `Disconnected _ | `Connecting _ -> assert false
      | `Connected con ->
        handle_incoming
          ~logfun:t.logfun
          ~ip:(Unix.Inet_addr.to_string t.remote_ip)
          ~con
          ~extend_data_needs_raw:false
          ~extend_disconnect:(fun n e ->
            Tail.extend t.messages
              (Control (Disconnect (n, Exn.sexp_of_t e))))
          ~extend_parse_error:(fun n e ->
            Tail.extend t.messages
              (Control (Parse_error (n, e))))
          ~extend_data:(fun x _ ->
            Tail.extend t.messages (Data x))
    ;;


    let flushed t =
      match t.con with
      | `Disconnected _
      | `Connecting _  -> `Flushed
      | `Connected con ->
        if 0 = Writer.bytes_to_write con.writer
        then `Flushed
        else `Pending (Writer.flushed_time con.writer)
    ;;

    let listen t = Tail.collect t.messages

    let listen_ignore_errors ?(stop = Deferred.never ()) t =
      let s = Stream.take_until (listen t) stop in
      Stream.filter_map_deprecated s ~f:(function
        | Control _ -> None
        | Data x -> Some x.data)

    let internal_send t msg =
      match t.con with
      | `Disconnected _ | `Connecting _ -> return `Dropped
      | `Connected con ->
        send ~logfun:t.logfun ~name:t.expected_remote_name ~now:t.now con msg

    let send_q t =
      Queue.iter t.queue ~f:(fun (d, i) ->
        upon (internal_send t d) (Ivar.fill i));
      Queue.clear t.queue

    let is_connected t =
      match t.con with
      | `Disconnected _
      | `Connecting _ -> false
      | `Connected _ -> true

    let purge_queue t =
      Queue.iter t.queue ~f:(fun (_, i) -> Ivar.fill i `Dropped);
      Queue.clear t.queue;
    ;;

    let try_connect t =
      let earliest_connect_time =
        match t.con with
        | `Connected _ | `Connecting _ -> assert false
        | `Disconnected last_disconnect ->
          Time.max (t.now ())
            (Time.add last_disconnect wait_after_connect_failure)
      in
      let disconnected () =
        t.con <- `Disconnected (t.now ());
        purge_queue t;
        return `Stop_connecting
      in
      let stop_connecting = Ivar.create () in
      t.con <- `Connecting stop_connecting;
      let retry_wait e =
        t.last_connect_error <- Some e;
        purge_queue t;
        Clock.with_timeout wait_after_connect_failure (Ivar.read stop_connecting)
        >>= function
        | `Result () -> disconnected ()
        | `Timeout -> return `Reconnect
      in
      let rec loop () =
        connect_internal t ~handler:(fun res ->
          match t.con with
          | `Disconnected _ | `Connected _ -> assert false
          | `Connecting stop_connecting' ->
            assert (phys_equal stop_connecting' stop_connecting);
            if Ivar.is_full stop_connecting then
              disconnected ()
            else begin
              match res with
              | Error e ->
                retry_wait e
              | Ok (con, credentials) ->
                t.last_connect_error <- None;
                t.con <- `Connected con;
                Tail.extend t.messages
                  (Control (Connect (con.remote_name, `credentials credentials)));
                Ivar.fill t.connect_complete ();
                t.connect_complete <- Ivar.create ();
                send_q t;
                handle_incoming t
                >>= fun () ->
                disconnected ()
            end)
        >>= function
        | `Connect_error e ->
          (* socket connect failed, try again *)
          begin retry_wait (Error.of_exn e)
            >>= function
            | `Stop_connecting -> Deferred.unit
            | `Reconnect -> loop ()
          end
        | `Handler_error e ->
          (* handler threw an exception *)
          t.con <- `Disconnected (t.now ());
          purge_queue t;
          raise e
        | `Ok `Reconnect -> loop ()
        | `Ok `Stop_connecting -> Deferred.unit
      in
      don't_wait_for (Clock.at earliest_connect_time >>= loop)
    ;;


    let send =
      let push t d =
        let i = Ivar.create () in
        Queue.enqueue t.queue (d, i);
        Ivar.read i
      in
      fun t d ->
        match t.con with
        | `Connecting _ -> push t d
        | `Disconnected _ ->
          let flush = push t d in
          try_connect t;
          flush
        | `Connected _ ->
          if Queue.is_empty t.queue
          then internal_send t d
          else begin
            let flush = push t d in
            send_q t;
            flush
          end
    ;;

    let close_connection t =
      match t.con with
      | `Disconnected _ -> ()
      | `Connecting stop_connecting ->
        (* drop the messages now, do not wait until the next async cycle, because a
           [send] might follow *)
        purge_queue t;
        Ivar.fill_if_empty stop_connecting ();
      | `Connected con -> Connection.kill con
    ;;

    let connect t =
      match t.con with
      | `Connected _ -> Deferred.unit
      | `Connecting _ ->
        Ivar.read t.connect_complete
      | `Disconnected _ ->
        try_connect t;
        Ivar.read t.connect_complete
    ;;

    let send_ignore_errors t d = ignore (send t d)

    let state t =
      match t.con with
      | `Disconnected _ -> `Disconnected
      | `Connecting _ -> `Connecting
      | `Connected _ -> `Connected
    ;;

    let create0
          ?logfun
          ?(now = Scheduler.cycle_start)
          ?(check_remote_name = true)
          ?(credentials = "")
          ~ip ~port ~expected_remote_name my_name =
      { remote_ip            = Unix.Inet_addr.of_string ip
      ; remote_port          = port
      ; logfun
      ; expected_remote_name
      ; check_remote_name
      ; my_name
      ; queue                = Queue.create ()
      ; messages             = Tail.create ()
      ; con                  = `Disconnected Time.epoch
      ; connect_complete     = Ivar.create ()
      ; now
      ; last_connect_error   = None
      ; credentials
      }
    ;;

    let create' = create0

    let create = create0 ?credentials:None

    let last_connect_error t = t.last_connect_error
  end
end

module Repeater
    (To_server_msg : Datum)
    (To_client_msg : Datum)
    (Server_name : Name)
    (Client_name : Name)
    (Mode : Mode) =
struct

  module Repeater_hook = struct
    type 'msg t =
      { on_error : Repeater_error.t -> unit
      ; on_data  : msg:'msg -> raw_msg:Bigsubstring.t -> unit
      }
  end

  module Z = Make (struct
      module To_client_msg = To_client_msg
      module To_server_msg = To_server_msg
      module Client_name = Client_name
      module Server_name = Server_name
      module Mode = Mode
    end)

  module Server = Z.Server

  (* restrict functions from client so that we don't call send functions that can initiate
     connection *)
  module Client = struct
    module C = Z.Client
    type t = C.t
    open C
    let is_connected = is_connected

    let connect t ~handler =
      (* This is not exposed to clients.  It is only called in [start] function
         below so it is called only once per [Client.t].  Hence the assertion below.
      *)
      assert (not (C.is_connected t));
      t.con <- `Connecting (Ivar.create ());
      connect_internal t
        ~handler:(fun res ->
          begin match res with
          | Error _     -> t.con <- `Disconnected (t.now ())
          | Ok (conn,_) -> t.con <- `Connected conn
          end;
          handler res)
    ;;

    let close_connection = close_connection
    let create = create0

    let send_ignore_errors t msg =
      if is_connected t then send_ignore_errors t msg
    ;;
  end

  module Connection = Z.Connection

  type repeater_to_server_side = < send : To_server_msg.t;
                                   recv : To_client_msg.t;
                                   my_name : Client_name.t;
                                   remote_name : Server_name.t >

  type repeater_to_client_side = repeater_to_server_side flipped

  type ('state, 'send, 'recv) filter =
    'recv -> state:'state
    -> client_name:Client_name.t
    -> server_name:Server_name.t
    -> ('send, 'recv) Repeater_hook_result.t

  type t =
    { server        : Server.t
    ; server_ip     : string
    ; server_port   : int
    ; server_name   : Server_name.t
    ; repeater_name : string
    ; clients       : Client.t Client_name.Table.t
    ; is_client_allowed : Client_name.t -> bool
    }

  let active_clients t =
    Hashtbl.fold t.clients ~init:[] ~f:(fun ~key:client_name ~data:client acc ->
      if Client.is_connected client
      && Server.client_is_connected t.server client_name
      then client_name::acc
      else acc)
  ;;

  let create
        ?is_client_ip_authorized
        ~is_client_allowed
        ~repeater_name
        ~listen_port
        ~server_ip
        ~server_port
        ~server_name =
    let equal3 a b c = (a = b && b = c) in
    if
      not (equal3
             To_server_msg.low_version
             To_server_msg.prod_version
             To_server_msg.test_version)
      || not (equal3
                To_client_msg.low_version
                To_client_msg.prod_version
                To_client_msg.test_version)
    then begin
      let of_v = Version.to_int in
      failwithf "Cannot create a repeater with multiple versions. \
                 Server: %d/%d/%d Client: %d/%d/%d"
        (of_v To_server_msg.low_version) (of_v To_server_msg.prod_version)
        (of_v To_server_msg.test_version) (of_v To_client_msg.low_version)
        (of_v To_client_msg.prod_version) (of_v To_client_msg.test_version)
        ()
    end;
    Server.create' ?is_client_ip_authorized ~listen_port
      ~credentials:repeater_name
      server_name
    >>| fun server ->
    { server
    ; clients       = Client_name.Table.create ()
    ; server_ip
    ; server_port
    ; server_name
    ; repeater_name
    ; is_client_allowed
    }
  ;;

  module Connection_side : sig
    type _ t =
      | Repeater_to_server : repeater_to_server_side t
      | Repeater_to_client : repeater_to_client_side t

    val to_poly : _ t -> [ `repeater_to_server | `repeater_to_client ]

    val flip : 'a t -> 'a flipped t

  end = struct

    type _ t =
      | Repeater_to_server : repeater_to_server_side t
      | Repeater_to_client : repeater_to_client_side t

    let to_poly (type a) (t : a t) =
      match t with
      | Repeater_to_server -> `repeater_to_server
      | Repeater_to_client -> `repeater_to_client

    let flip : type a b c d .
      < send :a; recv:b; my_name:c; remote_name:d> t ->
      < send :b; recv:a; my_name:d; remote_name:c> t =
      function
      | Repeater_to_client -> Repeater_to_server
      | Repeater_to_server -> Repeater_to_client
  end

  let send_msg
        msg
        ~(conn_side : 'a Connection_side.t)
        ~(conn : 'a Z.Connection.t)
        ~on_send_error =
    if not (Writer.is_closed conn.writer)
    then begin
      let now = Scheduler.cycle_start in
      match Z.send_no_flush ~logfun:None ~now ~name:conn.my_name conn msg with
      | `Sent -> ()
      | `Not_sent ->
        on_send_error conn_side Repeater_error.Marshaling_error
    end
  ;;

  (* create a [Repeater_hook.t] that is used to filter messages on one side of paired
     connections. The side is indicated with [conn_side] argument. [make_hook] has a
     reference to both connection and based on [on_data] it can send messages to both
     sides. Assumption is that Pass_on is the most common result of the application level
     filter. In that case, we just blit the bytes over to the other side instead of
     invoking more complicated [send_msg] function.

     Type annotations hopefully reduce some confusion about which side of the paired
     connections we are dealing with and prevent sending messages over the wrong
     connection. *)
  let make_hook
        ~(conn_side:(< send      : 'send
                    ; recv       : 'recv
                    ; my_name    : _
                    ;remote_name : _ > as 'side) Connection_side.t)
        ~(conn:'side Connection.t)
        ~(paired_conn:'side flipped Connection.t)
        ~client_name
        ~server_name
        ~app_on_error
        ~(app_msg_filter : ('state, 'send, 'recv) filter)
        ~(state : 'state)
    =
    let on_error' conn_side error =
      app_on_error ~client_name ~server_name ~state
        (Connection_side.to_poly conn_side) error
    in
    let on_send_error = on_error' in
    let send_forward msg =
      send_msg msg ~conn_side:(Connection_side.flip conn_side)
        ~conn:paired_conn
        ~on_send_error
    in
    let send_back msg =
      send_msg msg ~conn_side ~conn ~on_send_error
    in
    let forward_bytes (raw_msg:Bigsubstring.t) =
      if not (Writer.is_closed paired_conn.writer) then
        Writer.write_bigsubstring paired_conn.writer raw_msg
    in
    let on_data ~msg:{ Read_result. data; _ } ~raw_msg =
      match
        (app_msg_filter data ~state ~client_name ~server_name
         : (_,_) Repeater_hook_result.t)
      with
      | Do_nothing -> ()
      | Send msg -> send_forward msg
      | Pass_on -> forward_bytes raw_msg
      | Pass_on_and_send_back msgs ->
        forward_bytes raw_msg;
        List.iter msgs ~f:(fun msg -> send_back msg);
      | Send_back msgs ->
        List.iter msgs ~f:(fun msg -> send_back msg);
    in
    let on_error error = on_error' conn_side error in
    { Repeater_hook.
      on_error
    ; on_data
    }
  ;;

  (* [start] establishes TCP server handler that will try to establish connection to the
     server whenever a client connects to the repeater. We first call functions to get two
     [Connection.t]s and only then we start processing data going over the
     connections. This makes it easy to create two [Repeater_hook.t]s that have access to
     both connections and that filter data based on provided application filters.

     We avoid sending Hello to the client before the connection to the server is
     established because that will cause client to think it connected. If establishing
     connection to server then fails it would lead to noisy Connect/Disconnect on the
     client side. Only when a connection to the server is established we negotiate with
     the client.

     We need to do a slightly hacky thing of reading the Hello from client in order to get
     the client's name that we then send to the server.

     After both connections are established we create hooks and run [handle_incoming] on
     both sides.
  *)
  let start t
        ~(on_connect : Client_name.t -> 'state Or_error.t)
        ~(to_server_msg_filter : ('state, To_client_msg.t, To_server_msg.t) filter)
        ~(to_client_msg_filter : ('state, To_server_msg.t, To_client_msg.t) filter)
        ~(on_error : client_name:Client_name.t -> server_name:Server_name.t ->
          state:'state ->
          [ `repeater_to_server | `repeater_to_client ] -> Repeater_error.t -> unit)
        ~(on_connecting_error:
            client_name:Client_name.t -> server_name:Server_name.t -> Error.t -> unit)
    =
    let (_ :  (Client_name.t, To_server_msg.t) Server_msg.t Stream.t) =
      Server.listen' t.server ~handler:(fun addr reader writer ->
        (* we need to read Hello from the client to get the client name *)
        Reader.read_bin_prot reader Z.Hello.bin_reader_t
        >>= function
        | `Eof -> Deferred.unit
        | `Ok client_hello ->
          let client_name = Client_name.of_string client_hello.name in
          let server_name = t.server_name in
          let error_connecting e =
            on_connecting_error ~client_name ~server_name e;
            Deferred.unit
          in
          if not (t.is_client_allowed client_name) then begin
            let e =
              Error.create "client not allowed on repeater"
                client_name Client_name.sexp_of_t
            in
            error_connecting e
          end else begin
            let new_client =
              Client.create
                ~ip:t.server_ip
                ~port:t.server_port
                ~expected_remote_name:t.server_name
                ~credentials:t.repeater_name
                client_name
            in
            Client.connect new_client ~handler:(function
              | Error e ->
                error_connecting (Error.tag e ~tag:"error connecting repeater to server")
              | Ok (repeater_to_server_conn, _) ->
                Server.maybe_accept_client t.server addr reader writer
                  ~received_hello:client_hello ()
                >>= function
                | Error control_error ->
                  let e =
                    Error.create "error while accepting client connection"
                      control_error [%sexp_of: Client_name.t Server_msg.Control.t]
                  in
                  error_connecting e
                | Ok (repeater_to_client_conn, client_ip) ->
                  match on_connect client_name with
                  | Error e -> error_connecting (Error.tag e ~tag:"application on_connect error")
                  | Ok state ->
                    Hashtbl.set t.clients ~key:client_name ~data:new_client;
                    let handle_incoming conn_side ~conn ~paired_conn filter ip =
                      let hook =
                        make_hook
                          ~conn_side
                          ~conn
                          ~paired_conn
                          ~client_name
                          ~server_name
                          ~app_on_error:on_error
                          ~app_msg_filter:filter
                          ~state
                      in
                      Z.handle_incoming
                        ~logfun:None
                        ~ip
                        ~con:conn
                        ~extend_data_needs_raw:true
                        ~extend_parse_error:(fun _ msg -> hook.on_error (Parse_error msg))
                        ~extend_data:(fun msg raw_msg ->  hook.on_data ~msg ~raw_msg)
                        ~extend_disconnect:
                          (fun _ exn -> hook.on_error (Disconnect (Error.of_exn exn)))
                    in
                    Deferred.all_unit
                      [ handle_incoming
                          Repeater_to_client
                          ~conn:repeater_to_client_conn
                          ~paired_conn:repeater_to_server_conn
                          to_server_msg_filter
                          client_ip

                      ; handle_incoming
                          Repeater_to_server
                          ~conn:repeater_to_server_conn
                          ~paired_conn:repeater_to_client_conn
                          to_client_msg_filter
                          t.server_ip
                      ])
            >>= function
            | `Connect_error e ->
              error_connecting
                (Error.tag (Error.of_exn e) ~tag:"error connecting repeater to server")
            | `Handler_error e ->
              (* exception raised by our handler. just reraise *)
              raise e
            | `Ok () -> Deferred.unit
          end)
    in
    ()
  ;;

  let close_connection_from_client t client_name =
    Server.close t.server client_name;
    Option.iter (Hashtbl.find t.clients client_name) ~f:(fun client ->
      Client.close_connection client);
    Hashtbl.remove t.clients client_name;
  ;;

  let send_to_server_from t client_name msg =
    Option.iter (Hashtbl.find t.clients client_name)
      ~f:(fun client -> Client.send_ignore_errors client msg)
  ;;

  let send_from_all_clients t msg =
    Hashtbl.iter t.clients ~f:(fun client -> Client.send_ignore_errors client msg)
  ;;

  let send_to_all_clients t msg =
    Server.send_to_all_ignore_errors t.server msg
  ;;

  let tcp_server t = t.server.tcp_server

  let drop_new_clients t =
    Tcp.Server.set_drop_incoming_connections (tcp_server t) true
  ;;

  let accept_new_clients t =
    Tcp.Server.set_drop_incoming_connections (tcp_server t) false
  ;;

  let shutdown t =
    Server.shutdown t.server >>| fun () ->
    Hashtbl.iter t.clients ~f:Client.close_connection
  ;;
end

(** Helpers to make your types Datumable if they are binable. Works with up
    to 5 versions (easily extensible to more) *)
module Datumable_of_binable = struct
  module type T = sig type t end
  module type T_bin = sig type t [@@deriving bin_io] end

  module V (V : T) (T : T) = struct
    module type S = sig
      val of_v : V.t -> T.t option
      val to_v : T.t -> V.t option
    end
  end

  module Make_datumable5
      (Versions : Versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V5 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
      (V5_cvt : V(V5)(T).S)
    : Datumable with type datum = T.t =
  struct
    type t = T.t
    type datum = t

    include Versions
    let () = assert (low_version <= prod_version
                     && prod_version <= test_version
                     && test_version <= Version.add low_version 4)

    let alloc = bigsubstring_allocator ()

    let marshal ~to_v ~bin_size_t ~bin_write_t t =
      match to_v t with
      | None -> None
      | Some v ->
        let length = bin_size_t v in
        let bss = alloc length in
        let start = Bigsubstring.pos bss in
        let pos = bin_write_t (Bigsubstring.base bss) ~pos:start v in
        if pos - start <> length then
          failwithf "marshal failure: %d - %d <> %d" pos start length ();
        Some bss
    ;;

    let unmarshal ~of_v ~bin_read_t bss =
      let start = Bigsubstring.pos bss in
      let pos_ref = ref start in
      let result = bin_read_t (Bigsubstring.base bss) ~pos_ref in
      let length = Bigsubstring.length bss in
      if !pos_ref - start <> length then
        failwithf "unmarshal failure: %d - %d <> %d" !pos_ref start length ();
      of_v result
    ;;

    module F (VN : T_bin) (C : V(VN)(T).S) = struct
      let f =
        (fun t -> marshal ~to_v:C.to_v ~bin_size_t:VN.bin_size_t
                    ~bin_write_t:VN.bin_write_t t),
        (fun bss -> unmarshal ~of_v:C.of_v ~bin_read_t:VN.bin_read_t bss)
    end
    module F1 = F(V1)(V1_cvt)
    module F2 = F(V2)(V2_cvt)
    module F3 = F(V3)(V3_cvt)
    module F4 = F(V4)(V4_cvt)
    module F5 = F(V5)(V5_cvt)

    let funs = [| F1.f; F2.f; F3.f; F4.f; F5.f |]

    let lookup version =
      Result.try_with (fun () ->
        funs.(Version.to_int version - Version.to_int low_version))
    ;;

    let lookup_marshal_fun version = Result.map (lookup version) ~f:fst
    let lookup_unmarshal_fun version = Result.map (lookup version) ~f:snd
  end

  module type Pre_versions = sig
    val low_version : Version.t
    val prod_version : Version.t
  end

  module Five_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V5 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
      (V5_cvt : V(V5)(T).S)
    : Datumable with type datum = T.t =
    Make_datumable5
      (struct
        include Versions
        let test_version = Version.add low_version 4
      end)
      (T)
      (V1)(V2)(V3)(V4)(V5)
      (V1_cvt)(V2_cvt)(V3_cvt)(V4_cvt)(V5_cvt)
  ;;

  module Four_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V4 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
      (V4_cvt : V(V4)(T).S)
    : Datumable with type datum = T.t =
    Make_datumable5
      (struct
        include Versions
        let test_version = Version.add low_version 3
      end)
      (T)
      (V1)(V2)(V3)(V4)(V4)
      (V1_cvt)(V2_cvt)(V3_cvt)(V4_cvt)(V4_cvt)
  ;;

  module Three_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V3 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
      (V3_cvt : V(V3)(T).S)
    : Datumable with type datum = T.t =
    Make_datumable5
      (struct
        include Versions
        let test_version = Version.add low_version 2
      end)
      (T)
      (V1)(V2)(V3)(V3)(V3)
      (V1_cvt)(V2_cvt)(V3_cvt)(V3_cvt)(V3_cvt)
  ;;

  module Two_versions
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V2 : T_bin)
      (V1_cvt : V(V1)(T).S)
      (V2_cvt : V(V2)(T).S)
    : Datumable with type datum = T.t =
    Make_datumable5
      (struct
        include Versions
        let test_version = Version.add low_version 1
      end)
      (T)
      (V1)(V2)(V2)(V2)(V2)
      (V1_cvt)(V2_cvt)(V2_cvt)(V2_cvt)(V2_cvt)
  ;;

  module One_version
      (Versions : Pre_versions)
      (T : T)
      (V1 : T_bin)
      (V1_cvt : V(V1)(T).S)
    : Datumable with type datum = T.t =
    Make_datumable5
      (struct
        include Versions
        let test_version = low_version
      end)
      (T)
      (V1)(V1)(V1)(V1)(V1)
      (V1_cvt)(V1_cvt)(V1_cvt)(V1_cvt)(V1_cvt)
  ;;
end
