open Lwt.Infix
open Logs_syslog_lwt__Logs_syslog_lwt_common
open Logs_syslog

let tcp_tls_reporter
    ?hostname ip ?(port = 6514) ~cacert ~cert ~priv_key ?(truncate = 0)
    ?(framing = `Null) ?facility () =
  let sa = Lwt_unix.ADDR_INET (ip, port) in
  let tls = ref None in
  let m = Lwt_mutex.create () in
  X509_lwt.private_of_pems ~cert ~priv_key >>= fun priv ->
  X509_lwt.authenticator (`Ca_file cacert) >>= fun authenticator ->
  let conf = Tls.Config.client ~authenticator ~certificates:(`Single priv) () in
  (match hostname with
   | Some x -> Lwt.return x
   | None -> Lwt_unix.gethostname ()) >>= fun host ->
  let connect () =
    let sock = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
    Lwt_unix.(setsockopt sock SO_REUSEADDR true) ;
    Lwt_unix.(setsockopt sock SO_KEEPALIVE true) ;
    Lwt.catch
      (fun () ->
         Lwt_unix.connect sock sa >>= fun () ->
         Tls_lwt.Unix.client_of_fd conf sock >|= fun t ->
         tls := Some t ;
         Ok ())
      (function
         | Unix.Unix_error (e, f, _) ->
           let err =
             Printf.sprintf "error %s in function %s while connecting to %s:%d"
               (Unix.error_message e) f (Unix.string_of_inet_addr ip) port
           in
           Lwt.return @@ Error err
         | Tls_lwt.Tls_failure f ->
           let err = Tls.Engine.string_of_failure f in
           Lwt.return @@ Error (Printf.sprintf "TLS failure %s" err)
         | exn -> Lwt.fail exn)
  in
  let reconnect k msg =
    Lwt_mutex.lock m >>= fun () ->
    (match !tls with
     | None -> connect ()
     | Some _ -> Lwt.return (Ok ())) >>= function
    | Ok () -> Lwt_mutex.unlock m ; k msg
    | Error e ->
      Printf.eprintf "%s while sending syslog message\n%s %s\n"
        e (Ptime.to_rfc3339 (Ptime_clock.now ())) msg ;
      Lwt_mutex.unlock m ;
      Lwt.return_unit
  in
  connect () >>= function
  | Error e -> Lwt.return (Error e)
  | Ok () ->
    let rec send omsg =
      match !tls with
      | None -> reconnect send omsg
      | Some t ->
        let msg = Cstruct.of_string (frame_message omsg framing) in
        Lwt.catch
          (fun () -> Tls_lwt.Unix.write t msg)
          (function
            | Unix.Unix_error (e, f, _) ->
              tls := None ;
              let err = Unix.error_message e in
              Printf.eprintf "error %s in function %s, reconnecting\n" err f ;
              Lwt.catch
                (fun () -> Tls_lwt.Unix.close t)
                (fun _ -> Lwt.return_unit) >>= fun () ->
              reconnect send omsg
            | Tls_lwt.Tls_failure f ->
              tls := None ;
              Printf.eprintf "TLS error %s\n" (Tls.Engine.string_of_failure f) ;
              Lwt.catch
                (fun () -> Tls_lwt.Unix.close t)
                (fun _ -> Lwt.return_unit) >>= fun () ->
              reconnect send omsg
            | exn -> Lwt.fail exn)
    in
    at_exit (fun () -> match !tls with
        | None -> ()
        | Some tls -> Lwt.async (fun () -> Tls_lwt.Unix.close tls)) ;
    Lwt.return (Ok (syslog_report_common facility host truncate Ptime_clock.now
                                         send Syslog_message.encode))

(*
let main () =
  let lo = Unix.inet_addr_of_string "127.0.0.1" in
  tcp_tls_reporter lo
    ~cacert:"cacert.pem" ~cert:"client.pem" ~priv_key:"client.key" ()
  >>= function
  | Error e -> print_endline e ; Lwt.return_unit
  | Ok r ->
    Logs.set_reporter r ;
    Logs.set_level ~all:true (Some Logs.Debug) ;
    Logs_lwt.warn (fun l -> l "foobar") >>= fun () ->
    Logs_lwt.err (fun l -> l "bar foofoobar") >>= fun () ->
    Logs_lwt.info (fun l -> l "foofoobar") >>= fun () ->
    Logs_lwt.debug (fun l -> l "debug foofoobar")

let _ = Lwt_main.run (main ())
*)
