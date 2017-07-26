open Core
open Core_profiler_disabled

module Profiler = struct
  let is_enabled = true

  let safe_to_delay () =
    Common.maybe_do_slow_tasks Common.Offline_profiler ~reluctance:1;
    Protocol.Buffer.ensure_free 2048

  let dump_stats () =
    Protocol.Writer.dump_stats ()

  let configure
    ?don't_require_core_profiler_env
    ?offline_profiler_data_file
    ?online_print_time_interval_secs:_
    ?online_print_by_default:_
    () =
    Option.iter don't_require_core_profiler_env ~f:(fun () ->
      Check_environment.don't_require_core_profiler_env ());
    Option.iter offline_profiler_data_file ~f:(fun file ->
      Protocol.set_current_output_filename file);
  ;;
end

module Timer = struct
  type timer = Probe_id.t
  type t = timer
  type probe = t

  let create ~name =
    Check_environment.check_safety_exn ();
    let id = Probe_id.create () in
    Protocol.Writer.write_new_single id name Probe_type.Timer;
    id

  let record id =
    let n = Common.now Common.Offline_profiler ~reluctance:3 () in
    Protocol.Writer.write_timer_at id n

  module Group = struct
    type t = Probe_id.t

    let create ~name =
      Check_environment.check_safety_exn ();
      let id = Probe_id.create () in
      Protocol.Writer.write_new_group id name Probe_type.Timer;
      id

    let add_probe group ?(sources=[||]) ~name () =
      let id = Probe_id.create () in
      (* Note! sources : Point.t array = Point_id.t array *)
      Protocol.Writer.write_new_group_point ~group_id:group ~id name sources;
      id

    let reset group =
      let n = Common.now Common.Offline_profiler ~reluctance:2 () in
      Protocol.Writer.write_group_reset group n
  end
end

let%bench_module "Timer" = (module struct
  let () = Profiler.configure ()
             ~don't_require_core_profiler_env:()

  let timer = Timer.create ~name:"bench_timer"

  (* let group = Timer.Group.create "bench_timer_group" ()
   * let group_probe = Timer.Group.add_probe group "bench_timer_group_probe" *)

  let%bench "at" = Timer.record timer

  (* BENCH "group_probe_at" = Timer.Group.Probe.at group_probe
   *
   * BENCH "group_reset" = Timer.Group.reset group *)

  let () = Protocol.Writer.set_at_exit_handler `Disable
end)


module Probe = struct
  type probe = Probe_id.t
  type t = probe

  let create ~name ~units =
    Check_environment.check_safety_exn ();
    let id = Probe_id.create () in
    Protocol.Writer.write_new_single id name (Probe_type.Probe units);
    id

  let record id value =
    let n = Common.now Common.Offline_profiler ~reluctance:3 () in
    Protocol.Writer.write_probe_at id n value

  module Group = struct
    type t = Probe_id.t

    let create ~name ~units =
      Check_environment.check_safety_exn ();
      let id = Probe_id.create () in
      Protocol.Writer.write_new_group id name (Probe_type.Probe units);
      id

    let add_probe group ?(sources=[||]) ~name () =
      let id = Probe_id.create () in
      Protocol.Writer.write_new_group_point ~group_id:group ~id name sources;
      id

    let reset group =
      let n = Common.now Common.Offline_profiler ~reluctance:2 () in
      Protocol.Writer.write_group_reset group n
  end
end

let%bench_module "Probe" = (module struct
  let () = Profiler.configure ()
             ~don't_require_core_profiler_env:()

  let timer = Probe.create ~name:"bench_probe" ~units:Profiler_units.Seconds

  (* let group = Probe.Group.create "bench_probe_group" Profiler_units.Int
   * let group_probe = Probe.Group.add_probe group "bench_probe_group_probe" *)

  let%bench "at" = Probe.record timer 19827312

  (* BENCH "group_probe_at" = Probe.Group.Probe.at group_probe 123812
   *
   * BENCH "group_reset" = Probe.Group.reset group *)

  let () = Protocol.Writer.set_at_exit_handler `Disable
end)

module Delta_timer = struct
  type state = Time_ns.t
  type t =
    { probe : Probe.t
    ; mutable state : state
    ; mutable accum : int
    }

  let create ~name =
    { probe = Probe.create ~name ~units:Profiler_units.Nanoseconds
    ; state = Time_ns.epoch
    ; accum = 0
    }

  let diff n state =
    Time_ns.diff n state
    |> Time_ns.Span.to_int_ns

  (* If we calibrate on start, we get back the time before we started calibrating,
     and those 300ns will be included in the delta. *)
  let stateless_start _t = Common.now Common.Offline_profiler ~reluctance:4 ()
  let stateless_stop t state =
    (* Avoid calling Common.now () twice: *)
    let n = Common.now Common.Offline_profiler ~reluctance:2 () in
    let d = diff n state in
    Protocol.Writer.write_probe_at t.probe n d

  let start t =
    let n = Common.now Common.Offline_profiler ~reluctance:4 () in
    t.state <- n

  let accumulate t n =
    t.accum <- t.accum + (diff n t.state);
    t.state <- n

  let write_probe_at t n =
    Protocol.Writer.write_probe_at t.probe n t.accum;
    t.accum <- 0;
    t.state <- n

  let pause t =
    let n = Common.now Common.Offline_profiler ~reluctance:4 () in
    accumulate t n

  let record t =
    let n = Common.now Common.Offline_profiler ~reluctance:2 () in
    write_probe_at t n

  let stop t =
    let n = Common.now Common.Offline_profiler ~reluctance:2 () in
    accumulate t n;
    write_probe_at t n

  let wrap_sync t f x =
    let state = stateless_start t in
    let r =
      try
        f x
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync"
    in
    stateless_stop t state;
    r

  let wrap_sync2 t f x y =
    let state = stateless_start t in
    let r =
      try
        f x y
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync2"
    in
    stateless_stop t state;
    r

  let wrap_sync3 t f x y z =
    let state = stateless_start t in
    let r =
      try
        f x y z
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync3"
    in
    stateless_stop t state;
    r

  let wrap_sync4 t f x y z w =
    let state = stateless_start t in
    let r =
      try
        f x y z w
      with ex ->
        stateless_stop t state;
        Exn.reraise ex "Core_profiler Delta_timer.wrap_sync4"
    in
    stateless_stop t state;
    r

  (* let wrap_async t f x =
   *   let open Async in
   *   let state = start_async t in
   *   try_with ~run:`Now (fun () -> f x) >>= fun res ->
   *   stop_async t state;
   *   match res with
   *   | Ok x -> return x
   *   | Error ex -> Exn.reraise ex "Core_profiler Delta_timer.wrap_async" *)
end

let%bench_module "Delta_timer" = (module struct
  let () = Profiler.configure ()
             ~don't_require_core_profiler_env:()

  let delta = Delta_timer.create ~name:"unittest"
  let started = Delta_timer.stateless_start delta

  let%bench "start_async" = Delta_timer.stateless_start delta
  let%bench "stop_async" = Delta_timer.stateless_stop delta started
  let%bench "start" = Delta_timer.start delta
  let%bench "stop" = Delta_timer.stop delta
end)

let%bench_module "Delta_timer.wrap_sync" = (module struct
  let () = Profiler.configure ()
             ~don't_require_core_profiler_env:()

  let nop () = ()

  let wrapped_nop =
    let delta = Delta_timer.create ~name:"nop" in
    Delta_timer.wrap_sync delta nop

  let count_256 () =
    for _ = 1 to 256 do
      ()
    done

  let wrapped_count_256 =
    let delta = Delta_timer.create ~name:"count_256" in
    Delta_timer.wrap_sync delta count_256

  let%bench "nop" = nop ()
  let%bench "wrapped_nop" = wrapped_nop ()
  let%bench "count_256" = count_256 ()
  let%bench "wrapped_count_256" = wrapped_count_256 ()
end)

(* stateless Delta_probe does not support pausing *)
module Delta_probe = struct
  type state = int
  type t =
    { probe : Probe.t
    ; mutable state : state
    ; mutable accum : state
    }

  let create ~name ~units =
    { probe = Probe.create ~name ~units
    ; state = 0
    ; accum = 0
    }

  let stateless_start _t value = value
  let stateless_stop t state value = Probe.record t.probe (value - state)

  let start t value =
    t.state <- value
  let record t =
    Probe.record t.probe t.accum;
    t.accum <- 0
  let pause t value =
    t.accum <- t.accum + (value - t.state)
  let stop t value =
    pause t value;
    record t;

end

let%bench_module "Delta_probe" = (module struct
  let () = Profiler.configure ()
             ~don't_require_core_profiler_env:()

  let delta = Delta_probe.create ~name:"unittest" ~units:Profiler_units.Int
  let started = Delta_probe.stateless_start delta 123

  let%bench "start" = Delta_probe.start delta 123
  let%bench "stop" = Delta_probe.stop delta 456
  let%bench "start_async" = Delta_probe.stateless_start delta 123
  let%bench "stop_async" = Delta_probe.stateless_stop delta started 456
end)
