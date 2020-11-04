open Base
open Lwt
open Nsq

let nsqd_address = "localhost"

let nsqd_port = 32782

let lookupd_address = "localhost"

let lookupd_port = 32778

let make_handler name msg =
  Logs_lwt.debug (fun l -> l "(%s) Handled Body: %s" name (Bytes.to_string msg))
  >>= fun () -> return `Ok

let publish_error_backoff = 1.0

let publish_interval_seconds = 1.0

let test_publish () =
  let p =
    Result.ok_or_failwith
    @@ Producer.create (HostPort (nsqd_address, nsqd_port))
  in
  let rec loop () =
    let msg = Unix.gettimeofday () |> Float.to_string |> Bytes.of_string in
    Logs_lwt.debug (fun l -> l "Publishing: %s" (Bytes.to_string msg))
    >>= fun () ->
    Producer.publish p (Topic "Test") msg >>= function
    | Result.Ok _ -> Lwt_unix.sleep publish_interval_seconds >>= loop
    | Result.Error e ->
        Logs_lwt.err (fun l -> l "%s" e) >>= fun () ->
        Lwt_unix.sleep publish_error_backoff >>= loop
  in
  loop ()

let create_consumer ~mode chan_name handler =
  let config =
    Consumer.Config.create ~max_in_flight:100
      ~lookupd_poll_interval:(Seconds.of_float 60.0) ()
    |> Result.ok_or_failwith
  in
  let host_port =
    match mode with
    | `Nsqd -> Address.HostPort (nsqd_address, nsqd_port)
    | `Lookupd -> HostPort (lookupd_address, lookupd_port)
  in
  Consumer.create ~mode ~config [ host_port ] (Topic "Test") (Channel chan_name)
    handler

let setup_logging level =
  Logs.set_level level;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ())

let () =
  setup_logging (Some Logs.Debug);
  let consumer =
    create_consumer ~mode:`Nsqd "nsq_consumer" (make_handler "nsq")
  in
  let l_consumer =
    create_consumer ~mode:`Lookupd "lookupd_consumer" (make_handler "lookupd")
  in
  let running_consumers = List.map ~f:Consumer.run [ l_consumer; consumer ] in
  let p1 = test_publish () in
  Lwt_main.run @@ join (p1 :: running_consumers)
