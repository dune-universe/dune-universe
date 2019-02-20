open Base
open Lwt
open Nsq

let nsqd_address = "172.17.0.2"
let publish_error_backoff = 1.0
let to_publish = 500000
let log_interval = 1.0
let start = ref (Unix.gettimeofday ())
let published = ref 0
let concurrency = 5

let publish p =
  let rec loop () =
    let msg = Int.to_string !published |> Bytes.of_string in
    Producer.publish p (Topic "Test") msg >>= function
    | Result.Ok _ -> 
      Int.incr published;
      if !published >= to_publish
      then Caml.exit 0
      else loop ()
    | Result.Error e -> 
      Logs_lwt.err (fun l -> l "%s" e) >>= fun () ->
      Lwt_unix.sleep publish_error_backoff >>= fun () -> 
      loop ()
  in
  loop ()

let rate_logger () =
  let rec loop () =
    Lwt_unix.sleep log_interval >>= fun () ->
    let published = !published in
    let elapsed = (Unix.gettimeofday ()) -. !start in
    let per_sec = (Float.of_int published) /. elapsed in
    Logs_lwt.debug (fun l -> l "Published %d, %f/s" published per_sec) >>= fun () ->
    loop ()
  in
  loop ()

let setup_logging level =
  Logs.set_level level;
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ())

let () = 
  setup_logging (Some Logs.Debug);
  let p = Result.ok_or_failwith @@ Producer.create ~pool_size:concurrency (Host nsqd_address) in
  let publishers = List.init ~f:(fun _ -> publish p) concurrency in
  start := Unix.gettimeofday ();
  Lwt_main.run @@ join ((rate_logger ()) :: publishers)

