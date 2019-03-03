open Lwt.Infix

module Udp (C : Mirage_console_lwt.S) (CLOCK : Mirage_clock.PCLOCK) (STACK : Mirage_stack_lwt.V4) = struct
  module UDP = STACK.UDPV4

  let create c clock stack ~hostname dst ?(port = 514) ?(truncate = 65535) ?facility () =
    let dsts =
      Printf.sprintf "while writing to %s:%d" (Ipaddr.V4.to_string dst) port
    in
    Logs_syslog_lwt_common.syslog_report_common
      facility
      hostname
      truncate
      (* This API for PCLOCK is inconvenient (overengineered?) *)
      (fun () -> Ptime.v (CLOCK.now_d_ps clock))
      (fun s ->
         UDP.write ~dst ~dst_port:port (STACK.udpv4 stack) (Cstruct.of_string s) >>= function
         | Ok _ -> Lwt.return_unit
         | Error e ->
           Format.(fprintf str_formatter "error %a %s, message: %s"
                     UDP.pp_error e dsts s) ;
           C.log c (Format.flush_str_formatter ()))
      Syslog_message.encode
end

module Tcp (C : Mirage_console_lwt.S) (CLOCK : Mirage_clock.PCLOCK) (STACK : Mirage_stack_lwt.V4) = struct
  open Logs_syslog
  module TCP = STACK.TCPV4

  let create c clock stack ~hostname dst ?(port = 514) ?(truncate = 0) ?(framing = `Null) ?facility () =
    let tcp = STACK.tcpv4 stack in
    let f = ref None in
    let dsts =
      Printf.sprintf "while writing to %s:%d" (Ipaddr.V4.to_string dst) port
    in
    let m = Lwt_mutex.create () in
    let connect () =
      TCP.create_connection tcp (dst, port) >|= function
      | Ok flow -> f := Some flow ; Ok ()
      | Error e ->
        TCP.pp_error Format.str_formatter e ;
        Error (Format.flush_str_formatter ())
    in
    let reconnect k msg =
      Lwt_mutex.lock m >>= fun () ->
      (match !f with
       | None -> connect ()
       | Some _ -> Lwt.return (Ok ())) >>= function
      | Ok () -> Lwt_mutex.unlock m ; k msg
      | Error e ->
        Lwt_mutex.unlock m ;
        C.log c (Printf.sprintf "error %s, message %s" e msg)
    in
    let rec send omsg =
      match !f with
      | None -> reconnect send omsg
      | Some flow ->
        let msg = Cstruct.(of_string (frame_message omsg framing)) in
        TCP.write flow msg >>= function
        | Ok () -> Lwt.return_unit
        | Error e ->
          f := None ;
          TCP.pp_write_error Format.str_formatter e ;
          C.log c (Format.flush_str_formatter () ^ " " ^ dsts ^ ", reconnecting") >>= fun () ->
          reconnect send omsg
    in
    connect () >|= function
    | Ok () ->
      Ok (Logs_syslog_lwt_common.syslog_report_common
            facility
            hostname
            truncate
            (fun () -> Ptime.v (CLOCK.now_d_ps clock))
            send
            Syslog_message.encode)
    | Error e -> Error e
end
