open Logs_syslog

let syslog_report
    facility
    host
    len
    send
    (encode : ?len:int -> Syslog_message.t -> string) =
  let report src level ~over k msgf =
    let source = Logs.Src.name src in
    let timestamp = Ptime_clock.now () in
    let k tags ?header _ =
      let facility = match Logs.Tag.find Logs_syslog.facility tags with
      | None -> facility
      | facility -> facility
      in
      let msg =
        message ?facility ~host ~source ~tags ?header level timestamp (flush ())
      in
      send (encode ~len msg) ; over () ; k ()
    in
    msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
    Format.kfprintf (k tags ?header) ppf fmt
  in
  { Logs.report }

let udp_reporter
    ?(hostname = Unix.gethostname ())
    ip
    ?(port = 514)
    ?(truncate = 65535)
    ?facility () =
  let sa = Unix.ADDR_INET (ip, port) in
  let s = Unix.(socket PF_INET SOCK_DGRAM 0) in
  let rec send msg =
    let b = Bytes.of_string msg in
    try ignore(Unix.sendto s b 0 (String.length msg) [] sa) with
    | Unix.Unix_error (Unix.EAGAIN, _, _) -> send msg
    | Unix.Unix_error (e, f, _) ->
      Printf.eprintf "error in %s %s while sending to %s:%d\n%s %s\n"
        f (Unix.error_message e) (Unix.string_of_inet_addr ip) port
        (Ptime.to_rfc3339 (Ptime_clock.now ()))
        msg
  in
  syslog_report facility hostname truncate send Syslog_message.encode

type state =
  | Disconnected
  | Connecting
  | Connected of Unix.file_descr

let wait_time = 0.01

let conn_reporter sd st sa truncate frame encode hostname facility =
  let s = ref Disconnected in
  let connect () =
    let sock = Unix.(socket sd st 0) in
    Unix.(setsockopt sock SO_REUSEADDR true) ;
    Unix.(setsockopt sock SO_KEEPALIVE true) ;
    try
      Unix.connect sock sa ;
      s := Connected sock;
      Ok ()
    with
    | Unix.Unix_error (e, f, _) ->
      let endpoint =
        match sa with
        | Unix.ADDR_UNIX socket -> socket
        | Unix.ADDR_INET (ip, port) ->
            Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port
      in
      let err =
        Printf.sprintf "error %s in function %s while connecting to %s\n"
          (Unix.error_message e) f endpoint
      in
      Error err
  in
  let reconnect k msg =
    s := Connecting ;
    match connect () with
    | Ok () -> k msg
    | Error e -> Printf.eprintf "%s while sending syslog message\n%s %s\n"
                   e (Ptime.to_rfc3339 (Ptime_clock.now ())) msg
  in
  match connect () with
  | Error e -> Error e
  | Ok () ->
    let transmit =
      if st = Unix.SOCK_DGRAM then
        fun sock b len -> ignore(Unix.send sock b 0 len [])
      else
        fun sock b len ->
          let rec aux idx =
            let should = len - idx in
            let n = Unix.send sock b idx should [] in
            if n <> should then aux (idx + n)
          in
          aux 0
    in
    let rec send omsg = match !s with
      | Disconnected -> reconnect send omsg
      | Connecting -> let _ = Unix.select [] [] [] wait_time in send omsg
      | Connected sock ->
        let msg = frame omsg |> Bytes.of_string in
        try transmit sock msg (Bytes.length msg) with
        | Unix.Unix_error (Unix.EAGAIN, _, _) -> send omsg
        | Unix.Unix_error (e, f, _) ->
          let err = Unix.error_message e in
          Printf.eprintf "error %s in function %s, reconnecting\n" err f ;
          (try Unix.close sock with Unix.Unix_error _ -> ()) ;
          s := Disconnected ;
          reconnect send omsg
    in
    at_exit (fun () -> match !s with Connected x -> Unix.close x | _ -> ()) ;
    Ok (syslog_report facility hostname truncate send encode)

let tcp_reporter
    ?(hostname = Unix.gethostname ())
    ip
    ?(port = 514)
    ?(truncate = 0)
    ?(framing = `Null)
    ?facility () =
  let sa = Unix.ADDR_INET (ip, port) in
  let frame msg = frame_message msg framing in
  let encode = Syslog_message.encode in
  let sd = Unix.PF_INET in
  let st = Unix.SOCK_STREAM in
  conn_reporter sd st sa truncate frame encode hostname facility

let unix_reporter ?(socket = "/dev/log") ?truncate ?framing ?facility () =
  let truncate =
    match truncate with
    | Some truncate -> truncate
    | None -> if framing = None then 65536 else 0
  in
  let frame, st =
    match framing with
    | Some framing -> (fun msg -> frame_message msg framing), Unix.SOCK_STREAM
    | None -> (fun msg -> msg), Unix.SOCK_DGRAM
  in
  let sa = Unix.ADDR_UNIX socket in
  let encode = Syslog_message.encode_local in
  let sd = Unix.PF_UNIX in
  conn_reporter sd st sa truncate frame encode "localhost" facility
