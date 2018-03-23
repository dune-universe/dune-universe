open Core
open Async

type t =
  { mutable ok_client_to_server : unit Ivar.t
  ; mutable ok_server_to_client : unit Ivar.t
  ; proxy_server : Tcp.Server.inet
  }

let transfer_data t
      ~reader_from_client ~writer_to_client
      ~reader_from_server ~writer_to_server
  =
  let setup_writer w =
    Writer.set_raise_when_consumer_leaves w false;
    don't_wait_for
      (Writer.consumer_left w >>= fun () ->
       Writer.close w)
  in
  let transfer reader writer transfer_ok =
    don't_wait_for
      (Writer.close_started writer >>= fun () ->
       Reader.close reader);
    Reader.read_one_chunk_at_a_time reader
      ~handle_chunk:(fun bs ~pos ~len ->
        transfer_ok () >>= fun () ->
        if Writer.is_closed writer then
          return (`Stop ())
        else begin
          Writer.write_bigstring writer bs ~pos ~len;
          let writer_ok =
            if Writer.bytes_to_write writer > 1_000_000 then
              Deferred.any [Writer.flushed writer; Writer.close_started writer]
            else
              Deferred.unit
          in
          writer_ok >>| fun () -> `Continue
        end)
    >>= fun _ ->
    Deferred.all_unit [Reader.close reader; Writer.close writer]
  in
  setup_writer writer_to_server;
  setup_writer writer_to_client;
  Deferred.all_unit
    [ transfer reader_from_client writer_to_server (fun () -> Ivar.read t.ok_client_to_server)
    ; transfer reader_from_server writer_to_client (fun () -> Ivar.read t.ok_server_to_client)
    ]
;;

let create ~proxy_listen_port ~server_listen_port =
  let rec proxy_server =
    lazy begin
      Tcp.Server.create (Tcp.Where_to_listen.of_port proxy_listen_port)
        ~on_handler_error:`Raise
        (fun addr reader_from_client writer_to_client ->
           let proxy_addr = Socket.Address.Inet.to_host_and_port addr in
           Tcp.with_connection
             (Tcp.Where_to_connect.of_host_and_port
                {host = Host_and_port.host proxy_addr; port = server_listen_port})
             (fun _sock reader_from_server writer_to_server ->
                force t >>= fun t ->
                transfer_data t
                  ~reader_from_client ~writer_to_client
                  ~reader_from_server ~writer_to_server
             ))
    end
  and t =
    lazy begin
      force proxy_server
      >>| fun proxy_server ->
      { ok_client_to_server = Ivar.create ()
      ; ok_server_to_client = Ivar.create ()
      ; proxy_server
      }
    end
  in
  force t
  >>| fun t ->
  Ivar.fill t.ok_server_to_client ();
  Ivar.fill t.ok_client_to_server ();
  t
;;

let proxy_listen_port t = Tcp.Server.listening_on t.proxy_server

let stop_client_to_server t = t.ok_client_to_server <- Ivar.create ()
let stop_server_to_client t = t.ok_server_to_client <- Ivar.create ()

let allow_client_to_server t = Ivar.fill_if_empty t.ok_client_to_server ()
let allow_server_to_client t = Ivar.fill_if_empty t.ok_server_to_client ()

let close t = Tcp.Server.close ~close_existing_connections:true t.proxy_server
