open Containers
open Lwt
open Nsq

let nsqd_address = "172.17.0.2"
let lookupd_address = "172.17.0.1"

let make_handler name =
  (fun msg ->
     Logs_lwt.debug (fun l -> l "(%s) Handled Body: %s" name (Bytes.to_string msg)) >>= fun () ->
     return HandlerOK
  )

let publish_error_backoff = 1.0
let publish_interval_seconds = 1.0

let rec test_publish () =
  let p = Result.get_exn @@ Producer.create (Host nsqd_address) in
  let rec loop () =
    let msg = Unix.gettimeofday () |> string_of_float |> Bytes.of_string in
    Logs_lwt.debug (fun l -> l "Publishing: %s" (Bytes.to_string msg)) >>= fun () ->
    Producer.publish p (Topic "Test") msg >>= function
    | Result.Ok _ -> 
      Lwt_unix.sleep publish_interval_seconds >>= loop
    | Result.Error e -> 
      Logs_lwt.err (fun l -> l "%s" e) >>= fun () ->
      Lwt_unix.sleep publish_error_backoff >>= test_publish
  in
  loop ()

let create_consumer ~mode chan_name handler =
  let dc = Consumer.default_config () in
  let config = Consumer.{dc with max_in_flight = 100;} in
  Result.get_exn
    (Consumer.create 
       ~mode
       ~config
       [(Host nsqd_address)]
       (Topic "Test") 
       (Channel chan_name)
       handler)

let setup_logging level =
  Logs.set_level (Some Logs.Debug);
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ())

let () = 
  setup_logging (Some Logs.Debug);
  let consumer = create_consumer ~mode:Consumer.ModeNsqd  "nsq_consumer" (make_handler "nsq") in
  let l_consumer = create_consumer ~mode:Consumer.ModeLookupd "lookupd_consumer" (make_handler "lookupd") in
  let running_consumers = List.map Consumer.run [l_consumer;consumer] in
  let p1 = test_publish () in
  Lwt_main.run @@ join (p1 :: running_consumers)

