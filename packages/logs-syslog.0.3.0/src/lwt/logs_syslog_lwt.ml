open Lwt.Infix
open Logs_syslog_lwt_common
open Logs_syslog

let udp_reporter ?hostname ip ?(port = 514) ?(truncate = 65535) ?facility () =
  let sa = Lwt_unix.ADDR_INET (ip, port) in
  let s = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
  let send msg =
    Lwt.catch (fun () ->
        let b = Bytes.of_string msg in
        Lwt_unix.sendto s b 0 (String.length msg) [] sa >|= fun _ -> ())
      (function
        | Unix.Unix_error (e, f, _) ->
          Printf.eprintf "error in %s %s while sending to %s:%d\n%s %s\n"
            f (Unix.error_message e) (Unix.string_of_inet_addr ip) port
            (Ptime.to_rfc3339 (Ptime_clock.now ()))
            msg ;
          Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  (match hostname with
   | Some x -> Lwt.return x
   | None -> Lwt_unix.gethostname ()) >|= fun host ->
  syslog_report_common facility host truncate Ptime_clock.now send
                       Syslog_message.encode

let conn_reporter sd st sa truncate frame encode hostname facility =
  let s = ref None in
  let m = Lwt_mutex.create () in
  let connect () =
    let sock = Lwt_unix.(socket sd st 0) in
    Lwt_unix.(setsockopt sock SO_REUSEADDR true) ;
    Lwt_unix.(setsockopt sock SO_KEEPALIVE true) ;
    Lwt.catch
      (fun () -> Lwt_unix.connect sock sa >|= fun () -> s := Some sock ; Ok ())
      (function Unix.Unix_error (e, f, _) ->
         let endpoint =
           match sa with
           | Unix.ADDR_UNIX socket -> socket
           | Unix.ADDR_INET (ip, port) ->
               Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
         in
         let err =
           Printf.sprintf "error %s in function %s while connecting to %s"
             (Unix.error_message e) f endpoint
         in
         Lwt.return (Error err)
       | exn -> Lwt.fail exn)
  in
  let reconnect k msg =
    Lwt_mutex.lock m >>= fun () ->
    (match !s with
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
    let transmit =
      if st = Unix.SOCK_DGRAM then
        fun sock b len () -> Lwt_unix.send sock b 0 len [] >|= fun _ -> ()
      else
        fun sock b len () ->
          let rec aux idx =
            let should = len - idx in
            Lwt_unix.send sock b idx (len - idx) [] >>= fun n ->
              if n = should then
                Lwt.return_unit
              else
                aux (idx + n)
          in
          aux 0
    in
    let rec send omsg =
      match !s with
      | None -> reconnect send omsg
      | Some sock ->
        let msg = frame omsg |> Bytes.of_string in
        (Lwt.catch (transmit sock msg (Bytes.length msg))
           (function
             | Unix.Unix_error (e, f, _) ->
               s := None ;
               let err = Unix.error_message e in
               Printf.eprintf "error %s in function %s, reconnecting\n" err f ;
               Lwt.catch
                 (fun () -> Lwt_unix.close sock)
                 (function
                   | Unix.Unix_error _ -> Lwt.return_unit
                   | exn -> Lwt.fail exn) >>= fun () ->
               reconnect send omsg
             | exn -> Lwt.fail exn))
    in
    at_exit (fun () -> match !s with
        | None -> ()
        | Some x -> Lwt.async (fun () -> Lwt_unix.close x)) ;
    Lwt.return (Ok (syslog_report_common facility hostname truncate
                                         Ptime_clock.now send encode))

let tcp_reporter ?hostname ip ?(port = 514) ?(truncate = 0) ?(framing = `Null) ?facility () =
  let sa = Lwt_unix.ADDR_INET (ip, port) in
  let frame msg = frame_message msg framing in
  let encode = Syslog_message.encode in
  (match hostname with
   | Some x -> Lwt.return x
   | None -> Lwt_unix.gethostname ()) >>= fun host ->
  conn_reporter Unix.PF_INET Unix.SOCK_STREAM sa truncate frame encode host facility

let unix_reporter ?(socket = "/dev/log") ?truncate ?framing ?facility () =
  let truncate =
    match truncate with
    | Some truncate -> truncate
    | None -> if framing = None then 65536 else 0
  in
  let frame, socket_type =
    match framing with
    | Some framing -> (fun msg -> frame_message msg framing), Unix.SOCK_STREAM
    | None -> (fun msg -> msg), Unix.SOCK_DGRAM
  in
  let sa = Unix.ADDR_UNIX socket in
  let encode = Syslog_message.encode_local in
  conn_reporter Unix.PF_UNIX socket_type sa truncate frame encode "localhost" facility
