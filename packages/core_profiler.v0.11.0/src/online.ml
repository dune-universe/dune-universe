open Core
open Core_profiler_disabled

let debug = false

type row =
  | Count_only of int
  | Stats of Fstats.t

let row_stats = function
  | Count_only _ -> None
  | Stats st -> Some st

let row_is_empty = function
  | Count_only n -> n = 0
  | Stats st -> Fstats.samples st = 0

let all_rows : ((unit -> row) * Profiler_units.t) String.Map.t ref = ref String.Map.empty

let columns =
  let fstats_count (_name, _units, row) =
    let count =
      match row with
      | Count_only n -> n
      | Stats st -> Fstats.samples st
    in
    Profiler_units.format_int Profiler_units.Int count
  in
  let fstats_fget getter (_name, units, row) =
    let open Option.Monad_infix in
    row_stats row
    >>| getter
    >>= Float.iround_nearest
    >>| Profiler_units.format_int units
    |> Option.value ~default:""
  in
  Textutils.Ascii_table.(
    [ Column.create ~align:Left  "name"  fst3
    ; Column.create ~align:Right "count" fstats_count
    ; Column.create ~align:Right "sum"   (fstats_fget Fstats.total)
    ; Column.create ~align:Right "mean"  (fstats_fget Fstats.mean)
    ; Column.create ~align:Right "min"   (fstats_fget Fstats.min)
    ; Column.create ~align:Right "max"   (fstats_fget Fstats.max)
    ; Column.create ~align:Right "stdev" (fstats_fget Fstats.stdev)
    ]
  )

(* If we ever create an online probe we expect this will be set. *)
let we_are_using_online_profiler = ref false

let online_profiler_is_used () =
  we_are_using_online_profiler := true

(* Used for benchmarks, tests etc where we don't what the online outputs, we can disable
   the printing by setting this. *)
let internal_disable_print = ref false

(* Printing configuration supplied by the user. *)
let print_enabled =
  let env = "PRINT_ENABLED" in
  let v =
    match Check_environment.get_var env with
    | Some "true" -> true
    | Some "false" -> false
    | Some v ->
      Core.printf "Unknown value for %s for %s, use true or false\n%!" v env;
      true
    | None -> true
  in
  ref v

(* Printing configuration supplied by the user. *)
let print_interval =
  let env = "PRINT_INTERVAL" in
  let v =
    match Check_environment.get_var env with
    | Some v -> Int.of_string v
    | None -> 1
  in
  ref v

let online_print () =
  if not !internal_disable_print && !we_are_using_online_profiler then begin
    Core.printf !"%{Time}\n%!" (Time.now ());
    let table =
      Map.fold_right
        !all_rows
        ~init:[]
        ~f:(fun ~key:name ~data:(row_fn, units) acc ->
          let row = row_fn () in
          if row_is_empty row
          then acc
          else (name, units, row) :: acc
        )
    in
    if not (List.is_empty table)
    then begin
      Textutils.Ascii_table.output
        ~oc:Out_channel.stdout
        ~limit_width_to:150
        columns
        table;
      Out_channel.flush Out_channel.stdout
    end
  end

let maybe_print =
  let last_print = ref (Common.now_no_calibrate ()) in
  fun () ->
    if !print_enabled then begin
      let now = Common.now_no_calibrate () in
      let diff =
        Time_ns.diff now !last_print
        |> Time_ns.Span.to_int_sec
      in
      if debug then
        Core.printf "print_interval = %d, diff = %d\n"
          !print_interval diff;
      if diff >= !print_interval then begin
        last_print := now;
        online_print ()
      end
    end

let add_print_to_slow_tasks =
  let once = ref false in
  fun () ->
    if not !once then begin
      Common.add_slow_task Common.Online_profiler maybe_print;
      once := true
    end

let () =
  at_exit online_print

let add_row  : string -> (unit -> row) -> Profiler_units.t -> unit =
  add_print_to_slow_tasks ();
  fun name fn units ->
    all_rows := Map.set !all_rows ~key:name ~data:(fn, units)

let safe_to_delay () =
  Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:1

module Profiler = struct
  let is_enabled = true

  let safe_to_delay = safe_to_delay
  let dump_stats = online_print

  let configure
    ?don't_require_core_profiler_env
    ?offline_profiler_data_file:_
    ?online_print_time_interval_secs
    ?online_print_by_default
    () =
    Option.iter don't_require_core_profiler_env ~f:(fun () ->
      Check_environment.don't_require_core_profiler_env ());
    Option.iter online_print_by_default ~f:(fun bool ->
      print_enabled := bool);
    Option.iter online_print_time_interval_secs ~f:(fun secs ->
      print_interval := secs)
  ;;
end

module Timer = struct
  module Single = struct
    type t =
      { name : string
      ; mutable count : int
      }

    let create name () =
      online_profiler_is_used ();
      Check_environment.check_safety_exn ();
      let t = { name; count = 0 } in
      add_row name (fun () -> Count_only t.count) Profiler_units.Int;
      t

    let record t =
      t.count <- t.count + 1;
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3
  end
  module Raw_group = struct
    type t =
      { name : string
      (* [session] is initialised to 0 *)
      ; mutable session : int
      }

    let create ~name =
      online_profiler_is_used ();
      { name; session = 0 }

    let reset group =
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:2;
      group.session <- group.session + 1
  end
  module Group_probe = struct
    type t =
      { name : string
      ; group : Raw_group.t
      ; sources : (t * Fstats.t) array
      (* [session] is initialised to -1 *)
      ; mutable session : int
      (* [last_time] is initalised to the epoch; the value won't be used since session
         won't match *)
      ; mutable last_time : Time_ns.t
      }

    let record t =
      let n = Common.now Common.Online_profiler ~reluctance:4 () in
      let gsession = t.group.session in
      for i = 0 to (Array.length t.sources - 1) do
        let (src, stats) = t.sources.(i) in
        if src.session = gsession then
          Time_ns.diff n src.last_time
          |> Time_ns.Span.to_int_ns
          |> float
          |> Fstats.update_in_place stats
      done;
      t.last_time <- n;
      t.session <- gsession


    let create group ~sources ~name =
      online_profiler_is_used ();
      Check_environment.check_safety_exn ();
      let probe =
        { name
        ; group
        ; sources = Array.map sources ~f:(fun src -> (src, Fstats.create ()))
        ; last_time = Time_ns.epoch
        ; session = -1
        }
      in
      Array.iter probe.sources ~f:(fun (src, stats) ->
        let row_name = group.name ^ ":" ^ src.name ^ "," ^ name in
        add_row row_name (fun () -> Stats (Fstats.copy stats)) Profiler_units.Nanoseconds
      );
      probe
  end

  type t =
    | Single of Single.t
    | Group_probe of Group_probe.t
  type probe = t

  let create ~name =
    Single (Single.create name ())

  let record = function
    | Single t -> Single.record t
    | Group_probe t -> Group_probe.record t

  module Group = struct
    include Raw_group

    let add_probe t ?(sources=[||]) ~name () =
      let sources =
        Array.map sources ~f:(fun (src : probe) ->
          match src with
          | Single _ -> failwith "Probe sources must come from the same group"
          | Group_probe src ->
            if src.group <> t
            then failwith "Probe sources must come from the same group"
            else src);
      in
      Group_probe  (Group_probe.create t ~sources ~name)
  end
end

let%bench_module "Timer" = (module struct
  let timer = Timer.create ~name:"bench_timer"

  let group = Timer.Group.create ~name:"bench_timer_group"
  let group_probe0 = Timer.Group.add_probe group ~name:"bench_timer_group_probe0" ()
  let group_probe1 =
    Timer.Group.add_probe group ~name:"bench_timer_group_probe1" ()
      ~sources:[|group_probe0|]
  let group_probe2 =
    Timer.Group.add_probe group ~name:"bench_timer_group_probe2" ()
      ~sources:[|group_probe0; group_probe1|]

  let () =
    Timer.record group_probe0;
    Timer.record group_probe1;
    Timer.record group_probe2

  let%bench "at" = Timer.record timer
  let%bench "group_probe_at (0 sources)" = Timer.record group_probe0
  let%bench "group_probe_at (1 sources)" = Timer.record group_probe1
  let%bench "group_probe_at (2 sources)" = Timer.record group_probe2

  let group2 = Timer.Group.create ~name:"bench_timer_group2"

  let%bench "group_reset" = Timer.Group.reset group2

  let () = internal_disable_print := true
end)


module Probe = struct
  (* A probe doesn't need to know its name, so we can save an indirection. *)
  module Single = struct
    type t = Fstats.t

    let create name units =
      online_profiler_is_used ();
      Check_environment.check_safety_exn ();
      let t = Fstats.create () in
      add_row name (fun () -> Stats (Fstats.copy t)) units;
      t

    let record t value =
      Fstats.update_in_place t (float value);
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3
  end

  module Raw_group = struct
    type t =
      { name : string
      ; units : Profiler_units.t
        (* [session] is initialised to 0 *)
      ; mutable session : int
      }

    let create ~name ~units =
      online_profiler_is_used ();
      { name; units; session = 0 }

    let reset group =
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:2;
      group.session <- group.session + 1
  end

  module Group_probe = struct
    type t =
      { name : string
      ; group : Raw_group.t
      ; sources : (t * Fstats.t) array
      (* See [Timer.Group.Probe.t] *)
      ; mutable session : int
      ; mutable last_value : int
      }

    let create group ~sources ~name =
      online_profiler_is_used ();
      Check_environment.check_safety_exn ();
      let probe =
        { name
        ; group
        ; sources = Array.map sources ~f:(fun src -> (src, Fstats.create ()))
        ; last_value = 0
        ; session = -1
        }
      in
      Array.iter probe.sources ~f:(fun (src, stats) ->
        let row_name = group.name ^ ":" ^ src.name ^ "," ^ name in
        add_row row_name (fun () -> Stats (Fstats.copy stats)) group.units
      );
      probe

    let record t value =
      let gsession = t.group.session in
      (* Using Array.iter would cause allocation of a closure *)
      for i = 0 to (Array.length t.sources - 1) do
        let (src, stats) = t.sources.(i) in
        if src.session = gsession then
          Fstats.update_in_place stats (float (value - src.last_value))
      done;
      t.last_value <- value;
      t.session <- gsession;
      Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:4
  end

  type t =
    | Single of Single.t
    | Group_probe of Group_probe.t

  type probe = t

  let create ~name ~units =
    Single (Single.create name units)

  let record t value =
    match t with
    | Single t -> Single.record t value
    | Group_probe t -> Group_probe.record t value

  module Group = struct
    include Raw_group

    let add_probe t ?(sources=[||]) ~name () =
      let sources =
        Array.map sources ~f:(fun (src : probe) ->
          match src with
          | Single _ ->
            failwith "Probe sources must come from the same group"
          | Group_probe src ->
            if src.group <> t
            then failwith "Probe sources must come from the same group"
            else src
        )
      in
      Group_probe (Group_probe.create t ~sources ~name)
  end
end

let%bench_module "Probe" = (module struct
  let probe = Probe.create ~name:"bench_probe" ~units:Profiler_units.Seconds

  let group = Probe.Group.create ~name:"bench_probe_group" ~units:Profiler_units.Words
  let group_probe0 = Probe.Group.add_probe group ~name:"bench_probe_group_probe0" ()
  let group_probe1 =
    Probe.Group.add_probe group ~name:"bench_probe_group_probe1"
      ~sources:[|group_probe0|] ()
  let group_probe2 =
    Probe.Group.add_probe group ~name:"bench_probe_group_probe2"
      ~sources:[|group_probe0; group_probe1|] ()

  let () =
    Probe.record group_probe0 2;
    Probe.record group_probe1 3;
    Probe.record group_probe2 4

  let%bench "at" = Probe.record probe 10

  let%bench "group_probe_at (0 sources)" = Probe.record group_probe0 5
  let%bench "group_probe_at (1 sources)" = Probe.record group_probe1 6
  let%bench "group_probe_at (2 sources)" = Probe.record group_probe2 7

  let group2 = Probe.Group.create ~name:"bench_probe_group2" ~units:Profiler_units.Int

  let%bench "group_reset" = Probe.Group.reset group2

  let () = internal_disable_print := true
end)

(* stateless Delta_timer does not support pausing *)
module Delta_timer = struct
  type state = Time_ns.t
  type t =
    { name : string
    ; stats : Fstats.t
    ; mutable state : state
    ; mutable accum : int
    }

  let create ~name =
    let t =
      { name
      ; stats = Fstats.create ()
      ; state = Time_ns.epoch
      ; accum = 0
      }
    in
    online_profiler_is_used ();
    add_row name (fun () -> Stats (Fstats.copy t.stats)) Profiler_units.Nanoseconds;
    t

  let diff n state =
    Time_ns.diff n state
    |> Time_ns.Span.to_int_ns

  let update_in_place t d =
    Fstats.update_in_place t.stats (float d)
  let update_in_place_and_reset_accum t d =
    update_in_place t d;
    t.accum <- 0

  let stateless_start _ = Common.now Common.Online_profiler ~reluctance:4 ()
  let stateless_stop t state =
    let n = Common.now Common.Online_profiler ~reluctance:4 () in
    let d = diff n state in
    update_in_place t d

  let start t =
    t.state <- Common.now Common.Online_profiler ~reluctance:4 ()

  let pause t =
    let n = Common.now Common.Online_profiler ~reluctance:4 () in
    t.accum <- t.accum + (diff n t.state)

  let record t =
    update_in_place_and_reset_accum t t.accum;
    Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:2

  let stop t =
    pause t;
    update_in_place_and_reset_accum t t.accum

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
  let delta = Delta_timer.create ~name:"unittest"
  let started = Delta_timer.stateless_start delta

  let%bench "stateless_start" = Delta_timer.stateless_start delta
  let%bench "stateless_stop" = Delta_timer.stateless_stop delta started
  let%bench "start" = Delta_timer.start delta
  let%bench "stop" = Delta_timer.stop delta

  let () = internal_disable_print := true
end)

let%bench_module "Delta_timer.wrap_sync" = (module struct
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

  let () = internal_disable_print := true
end)

(* stateless Delta_probe does not support pausing *)
module Delta_probe = struct
  type state = int
  type t =
    { name : string
    ; stats : Fstats.t
    ; mutable state : state
    ; mutable accum : state
    }

  let create ~name ~units =
    let t =
      { name
      ; stats = Fstats.create ()
      ; state = 0
      ; accum = 0
      }
    in
    online_profiler_is_used ();
    add_row name (fun () -> Stats (Fstats.copy t.stats)) units;
    t

  let stateless_start _ value = value
  let stateless_stop  t state value =
    Fstats.update_in_place t.stats (float (value - state));
    Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3

  let start t value =
    t.state <- value

  let pause t value =
    t.accum <- t.accum + value - t.state

  let record t =
    Fstats.update_in_place t.stats (float t.accum);
    t.accum <- 0;
    Common.maybe_do_slow_tasks Common.Online_profiler ~reluctance:3

  let stop t value =
    pause t value;
    record t

end

let%bench_module "Delta_probe" = (module struct
  let delta = Delta_probe.create ~name:"unittest" ~units:Profiler_units.Int
  let started = Delta_probe.stateless_start delta 123

  let%bench "start" = Delta_probe.start delta 123
  let%bench "stop" = Delta_probe.stop delta 456
  let%bench "start_async" = Delta_probe.stateless_start delta 123
  let%bench "stop_async" = Delta_probe.stateless_stop delta started 456

  let () = internal_disable_print := true
end)
