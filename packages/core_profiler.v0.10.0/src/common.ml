open Core

type t =
  | Online_profiler
  | Offline_profiler
  | Any_profiler

let now_no_calibrate () =
  Time_stamp_counter.now ()
  |> Time_stamp_counter.to_time_ns

let%bench "now_no_calibrate" = now_no_calibrate ()

(* When we last ran the slow tasks *)
let last_slow_tasks = ref (now_no_calibrate ())

(* NB: Time_stamp_counter calibrates at startup *)
let slow_tasks : (t * (unit -> unit)) list ref = ref []
let slow_tasks_every_ns = 1_000_000_000

let add_slow_task kind f =
  slow_tasks := (kind, f) :: !slow_tasks

let () =
  add_slow_task Any_profiler
    (fun () -> Time_stamp_counter.Calibrator.calibrate ())

let maybe_do_slow_tasks' kind now reluctance =
  (* We don't want to pay for a [now] call to work out whether we should do slow tasks.
     If Time_stamp_counter gets so far out of sync with reality that the value below
     is not good enough to compare with values on the order of one second, then we have
     bigger problems, not least because Time_stamp_counter's EWMA isn't going to catch up
     quickly enough for the next measurement to be good. *)
  let diff =
    Time_ns.diff now !last_slow_tasks
    |> Time_ns.Span.to_int_ns
    |> abs
  in
  if diff > (slow_tasks_every_ns * reluctance) then begin
    List.iter !slow_tasks ~f:(fun (orig_kind, g) ->
      if orig_kind = kind || orig_kind = Any_profiler
      then g ());
    last_slow_tasks := now
  end


let now kind ~reluctance () =
  let x = now_no_calibrate () in
  maybe_do_slow_tasks' kind x reluctance;
  (* It is OK to take the value we were given /before/ calibration as "now" because
     Time_stamp_counter provides monotonicity and smoothness: it won't jump. *)
  x

let maybe_do_slow_tasks kind ~reluctance =
  maybe_do_slow_tasks' kind (now_no_calibrate ()) reluctance


let%bench "now" = now Any_profiler ~reluctance:1 ()
let%bench "maybe_do_slow_tasks (r=4)" = maybe_do_slow_tasks Any_profiler ~reluctance:4
