open Logs_syslog

let syslog_report_common
    facility
    host
    len
    now
    send
    (encode : ?len:int -> Syslog_message.t -> string) =
  let reporting = ref false in
  (* This is local mutable state to avoid reporting log messages while
     reporting a log message, which leads to out of memory (if the new log
     message is created before the reported one) or busy logging (if the new log
     message is created after the reported one) in cases where the code path
     is always the same (used to happen in tcpip where IPv4.write logged, which
     is used by the syslog reporters in `send` below).
     Avoiding this busy loop behaviour is done in an unsafe way: (local) mutable
     state (a bool ref), which is read and written without any lock. This works
     well in Lwt since there is no bind / blocking operation between the read
     and write of `reporting`. Should be revised with a mutex to guard the
     access to `reporting`. *)
  let report src level ~over k msgf =
    if !reporting then begin
      over () ; k ()
    end else begin
      reporting := true ;
      let source = Logs.Src.name src in
      let timestamp = now () in
      let k tags ?header _ =
        let facility = match Logs.Tag.find Logs_syslog.facility tags with
          | None -> facility
          | facility -> facility
        in
        let msg =
          message ?facility ~host ~source ~tags ?header level timestamp (flush ())
        in
        let bytes = encode ~len msg in
        let unblock () = over () ; Lwt.return_unit in
        Lwt.finalize (fun () -> send bytes) unblock |> Lwt.ignore_result ;
        reporting := false ; k ()
      in
      msgf @@ fun ?header ?(tags = Logs.Tag.empty) fmt ->
      Format.kfprintf (k tags ?header) ppf fmt
    end
  in
  { Logs.report }
