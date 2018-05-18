open Containers
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
    Lwt_unix.sleep log_interval >>= fun () ->
    let consumed = !consumed in
    let elapsed = (Unix.gettimeofday ()) -. !start in
    let per_sec = (float consumed) /. elapsed in
    Lwt_log.debug_f "Consumed %d, %f/s" consumed per_sec >>= fun () ->
    if consumed >= expected
    then exit 0
    else loop ()
  in
  loop ()

let setup_logging level =
  Lwt_log_core.default :=
    Lwt_log.channel
      ~template:"$(date).$(milliseconds) [$(level)] $(message)"
      ~close_mode:`Keep
      ~channel:Lwt_io.stdout
      ();
  Lwt_log_core.add_rule "*" level

let handler msg =
  consumed := !consumed + 1;
  return HandlerOK

let () = 
  setup_logging Lwt_log.Debug;
  let dc = Consumer.default_config () in
  let config = Consumer.{dc with max_in_flight = in_flight} in
  let consumer = 
    Result.get_exn  
      (Consumer.create
         ~mode:Consumer.ModeNsqd
         ~config
         [(Host nsqd_address)]
         (Topic "Test") 
         (Channel "benchmark")
         handler)
  in
  let consumer = Consumer.run consumer in
  let logger = rate_logger () in
  start := Unix.gettimeofday ();
  Lwt_main.run @@ join [logger; consumer]

