open Core
open Async
open Async_ssl.Std

let is_tls_url url = match Uri.scheme url with
  | Some "https"
  | Some "wss" -> true
  | _ -> false

let ssl_cleanup conn _flushed =
  Ssl.Connection.close conn

let ssl_connect ?version ?options ?(timeout=Time_ns.Span.of_int_sec 5) url r w =
  let net_to_ssl, net_to_ssl_w = Pipe.create () in
  let ssl_to_net_r, ssl_to_net = Pipe.create () in
  don't_wait_for (Pipe.transfer_id (Reader.pipe r) net_to_ssl_w) ;
  don't_wait_for (Pipe.transfer_id ssl_to_net_r (Writer.pipe w)) ;
  let app_to_ssl, client_w = Pipe.create () in
  let client_r, ssl_to_app = Pipe.create () in
  let hostname = Uri.host url in
  Clock_ns.with_timeout timeout
    (Ssl.client ?hostname ?version ?options ~app_to_ssl ~ssl_to_app ~net_to_ssl ~ssl_to_net ()) >>= function
  | `Timeout -> failwith "SSL handshake timeout"
  | `Result (Error e) -> Error.raise e
  | `Result (Ok conn) ->
    Reader.of_pipe (Info.createf "ssl_r") client_r >>= fun client_r ->
    Writer.of_pipe (Info.createf "ssl_w") client_w >>=
    fun (client_w, `Closed_and_flushed_downstream flushed) ->
    Monitor.detach_and_iter_errors (Writer.monitor w)
      ~f:(Monitor.send_exn (Writer.monitor client_w)) ;
    don't_wait_for begin
      Reader.close_finished client_r >>= fun () ->
      Reader.close r
    end ;
    don't_wait_for begin
      Writer.close_finished client_w >>= fun () ->
      Writer.close w
    end ;
    don't_wait_for begin
      Deferred.all_unit [ Reader.close_finished client_r ;
                          Writer.close_finished client_w ] >>| fun () ->
      ssl_cleanup conn flushed
    end ;
    return (conn, flushed, client_r, client_w)

let port_of_url url =
  match Uri.port url, Uri_services.tcp_port_of_uri url, Uri.scheme url with
  | Some p, _, _ -> p
  | None, Some p, _ -> p
  | None, None, Some "wss" -> 443
  | _ -> invalid_arg "no port in URL"

let connect
    ?version
    ?options
    ?socket
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?timeout url =
  let host = Option.value_exn ~message:"no host in URL" (Uri.host url) in
  let port = port_of_url url in
  Unix.Inet_addr.of_string_or_getbyname host >>= fun inet_addr ->
  Tcp.connect
    ?socket ?buffer_age_limit ?interrupt ?reader_buffer_size ?writer_buffer_size ?timeout
    (Tcp.Where_to_connect.of_inet_address (`Inet (inet_addr, port))) >>= fun (s, r, w) ->
  begin match is_tls_url url with
    | false -> return (s, None, r, w)
    | true ->
      ssl_connect ?version ?options url r w >>= fun (conn, _flushed, r, w) ->
      return (s, Some conn, r, w)
  end

let with_connection
    ?version
    ?options
    ?buffer_age_limit
    ?interrupt
    ?reader_buffer_size
    ?writer_buffer_size
    ?timeout url f =
  let host = Option.value_exn ~message:"no host in URL" (Uri.host url) in
  let port = port_of_url url in
  Unix.Inet_addr.of_string_or_getbyname host >>= fun inet_addr ->
  Tcp.with_connection
    ?buffer_age_limit ?interrupt ?reader_buffer_size ?writer_buffer_size ?timeout
    (Tcp.Where_to_connect.of_inet_address (`Inet (inet_addr, port)))
    begin fun s r w ->
      match is_tls_url url with
      | false -> f s None r w
      | true ->
        ssl_connect ?version ?options url r w >>= fun (conn, _flushed, r, w) ->
        Monitor.protect (fun () -> f s (Some conn) r w)
          ~finally:begin fun () ->
            Reader.close r >>= fun () ->
            Writer.close w
          end
    end

let listen_ssl
    ?buffer_age_limit ?max_connections
    ?max_accepts_per_batch
    ?backlog ?socket
    ?version ?options ?name ?allowed_ciphers
    ?ca_file ?ca_path ?verify_modes
    ~crt_file ~key_file
    ~on_handler_error listen_on f =
  Tcp.Server.create
    ?buffer_age_limit ?max_connections
    ?max_accepts_per_batch
    ?backlog ?socket ~on_handler_error listen_on begin fun s r w ->
    let ssl_to_net = Writer.pipe w in
    let net_to_ssl = Reader.pipe r in
    let app_to_ssl, client_write = Pipe.create () in
    let client_read, ssl_to_app = Pipe.create () in
    Reader.of_pipe
      (Info.of_string "read_pipe") client_read >>= fun r ->
    Writer.of_pipe
      (Info.of_string "writer_pipe") client_write >>= fun (w, _) ->
    Monitor.protect begin fun () ->
      Ssl.server ?version ?options ?name ?allowed_ciphers
        ?ca_file ?ca_path ~crt_file ~key_file ?verify_modes
        ~ssl_to_net ~net_to_ssl ~app_to_ssl ~ssl_to_app () |>
      Deferred.Or_error.ok_exn >>= fun c -> f s c r w
    end ~finally:begin fun () ->
      Pipe.close ssl_to_net ;
      Pipe.close_read net_to_ssl ;
      Pipe.close_read app_to_ssl ;
      Pipe.close_read client_read ;
      Reader.close r >>= fun () ->
      Writer.close w
    end
  end
