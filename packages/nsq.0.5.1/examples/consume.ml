open Base
open Lwt
open Nsq

let nsqd_address = "172.17.0.2"

let log_interval = 1.0

let start = ref (Unix.gettimeofday ())

let expected = 500000

let consumed = ref 0

let in_flight = 100

let rate_logger () =
  let rec loop () =
    Lwt_unix.sleep log_interval
    >>= fun () ->
    let consumed = !consumed in
    let elapsed = Unix.gettimeofday () -. !start in
    let per_sec = Float.of_int consumed /. elapsed in
    Logs_lwt.debug (fun l -> l "Consumed %d, %f/s" consumed per_sec)
    >>= fun () -> if consumed >= expected then Caml.exit 0 else loop ()
  in
  loop ()

let setup_logging level =
  Logs.set_level level ;
  Fmt_tty.setup_std_outputs () ;
  Logs.set_reporter (Logs_fmt.reporter ())

let handler _ =
  Int.incr consumed ;
  return `Ok

let () =
  setup_logging (Some Logs.Debug) ;
  let config =
    Consumer.Config.create ~max_in_flight:in_flight () |> Result.ok_or_failwith
  in
  let consumer =
    Consumer.create ~mode:`Nsqd ~config [Host nsqd_address] (Topic "Test")
      (Channel "benchmark") handler
  in
  let running = Consumer.run consumer in
  let logger = rate_logger () in
  start := Unix.gettimeofday () ;
  Lwt_main.run @@ join [logger; running]
