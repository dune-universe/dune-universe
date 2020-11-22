open Core
open Async
open Async_ssl.Std

let is_tls_url url =
  match Uri.scheme url with Some "https" | Some "wss" -> true | _ -> false

let ssl_connect ?version ?options ?(timeout = Time_ns.Span.of_int_sec 5) url r w
    =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net =
    Pipe.create_writer (fun r -> Pipe.transfer_id r (Writer.pipe w))
  in
  let app_to_ssl, client_w = Pipe.create () in
  let client_r, ssl_to_app = Pipe.create () in
  let hostname = Uri.host url in
  Clock_ns.with_timeout timeout
    (Ssl.client ?hostname ?version ?options ~app_to_ssl ~ssl_to_app ~net_to_ssl
       ~ssl_to_net ())
  >>= function
  | `Timeout -> failwith "SSL handshake timeout"
  | `Result (Error e) -> Error.raise e
  | `Result (Ok conn) ->
      Reader.of_pipe (Info.createf "ssl_r") client_r >>= fun client_r ->
      Writer.of_pipe (Info.createf "ssl_w") client_w
      >>= fun (client_w, `Closed_and_flushed_downstream flushed) ->
      Monitor.detach_and_iter_errors (Writer.monitor w)
        ~f:(Monitor.send_exn (Writer.monitor client_w));
      don't_wait_for
        (Reader.close_finished client_r >>= fun () -> Reader.close r);
      don't_wait_for
        (Writer.close_finished client_w >>= fun () -> Writer.close w);
      don't_wait_for
        ( Deferred.all_unit
            [ Reader.close_finished client_r; Writer.close_finished client_w ]
        >>| fun () -> Ssl.Connection.close conn );
      return (conn, flushed, client_r, client_w)

module T = struct
  module Address = Uri_sexp

  type t = {
    s : ([ `Active ], Socket.Address.Inet.t) Socket.t;
    ssl : Ssl.Connection.t option;
    r : Reader.t;
    w : Writer.t;
  }

  let create ?ssl s r w = { s; ssl; r; w }

  let close { ssl; r; w; _ } =
    Option.iter ~f:Ssl.Connection.close ssl;
    Writer.close w >>= fun () -> Reader.close r

  let close_finished { r; _ } = Reader.close_finished r

  let is_closed { r; _ } = Reader.is_closed r
end

include T
module Persistent = Persistent_connection_kernel.Make (T)

let port_of_url url =
  match (Uri.port url, Uri_services.tcp_port_of_uri url, Uri.scheme url) with
  | Some p, _, _ -> p
  | None, Some p, _ -> p
  | None, None, Some "wss" -> 443
  | _ -> invalid_arg "no port in URL"

let connect ?version ?options ?socket ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout url =
  let host = Option.value_exn ~message:"no host in URL" (Uri.host url) in
  let port = port_of_url url in
  Unix.Inet_addr.of_string_or_getbyname host >>= fun inet_addr ->
  Tcp.connect ?socket ?buffer_age_limit ?interrupt ?reader_buffer_size
    ?writer_buffer_size ?timeout
    (Tcp.Where_to_connect.of_inet_address (`Inet (inet_addr, port)))
  >>= fun (s, r, w) ->
  match is_tls_url url with
  | false -> return (create s r w)
  | true ->
      ssl_connect ?version ?options url r w >>= fun (ssl, _flushed, r, w) ->
      return (create ~ssl s r w)

let with_connection ?version ?options ?buffer_age_limit ?interrupt
    ?reader_buffer_size ?writer_buffer_size ?timeout url f =
  let host = Option.value_exn ~message:"no host in URL" (Uri.host url) in
  let port = port_of_url url in
  Unix.Inet_addr.of_string_or_getbyname host >>= fun inet_addr ->
  Tcp.with_connection ?buffer_age_limit ?interrupt ?reader_buffer_size
    ?writer_buffer_size ?timeout
    (Tcp.Where_to_connect.of_inet_address (`Inet (inet_addr, port)))
    (fun s r w ->
      match is_tls_url url with
      | false -> f (create s r w)
      | true ->
          ssl_connect ?version ?options url r w >>= fun (ssl, _flushed, r, w) ->
          Monitor.protect
            (fun () -> f (create ~ssl s r w))
            ~finally:(fun () -> Reader.close r >>= fun () -> Writer.close w) )

let listen_ssl ?version ?options ?name ?allowed_ciphers ?ca_file ?ca_path
    ?verify_modes ~crt_file ~key_file ?buffer_age_limit ?max_connections
    ?max_accepts_per_batch ?backlog ?socket ~on_handler_error listen_on f =
  Tcp.Server.create ?buffer_age_limit ?max_connections ?max_accepts_per_batch
    ?backlog ?socket ~on_handler_error listen_on (fun s r w ->
      let ssl_to_net = Writer.pipe w in
      let net_to_ssl = Reader.pipe r in
      let app_to_ssl, client_write = Pipe.create () in
      let client_read, ssl_to_app = Pipe.create () in
      Reader.of_pipe (Info.of_string "read_pipe") client_read >>= fun r ->
      Writer.of_pipe (Info.of_string "writer_pipe") client_write
      >>= fun (w, _) ->
      Monitor.protect
        (fun () ->
          Ssl.server ?version ?options ?name ?allowed_ciphers ?ca_file ?ca_path
            ~crt_file ~key_file ?verify_modes ~ssl_to_net ~net_to_ssl
            ~app_to_ssl ~ssl_to_app ()
          |> Deferred.Or_error.ok_exn
          >>= fun c -> f s c r w )
        ~finally:(fun () ->
          Pipe.close ssl_to_net;
          Pipe.close_read net_to_ssl;
          Pipe.close_read app_to_ssl;
          Pipe.close_read client_read;
          Reader.close r >>= fun () -> Writer.close w ) )
