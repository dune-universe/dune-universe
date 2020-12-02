open Lwt.Infix

module Main (C : Mirage_console.S) (CLOCK : Mirage_clock.PCLOCK) (T : Mirage_time.S) (S : Mirage_stack.V4V6) = struct
  module LU = Logs_syslog_mirage.Udp(C)(CLOCK)(S)

  let start c _clock _time s =
    let ip = Ipaddr.V4.of_string_exn "10.0.0.1" in
    let r = LU.create c s ~hostname:"MirageOS.example" ip () in
    Logs.set_reporter r ;
    Logs.set_level ~all:true (Some Logs.Debug) ;
    let rec go () =
      Logs_lwt.warn (fun l -> l "foobar") >>= fun () ->
      Logs_lwt.err (fun l -> l "bar foofoobar") >>= fun () ->
      Logs_lwt.info (fun l -> l "foofoobar") >>= fun () ->
      Logs_lwt.debug (fun l -> l "debug foofoobar") >>= fun () ->
      T.sleep_ns (Duration.of_sec 1) >>= fun () ->
      go ()
    in
    go ()

  end
