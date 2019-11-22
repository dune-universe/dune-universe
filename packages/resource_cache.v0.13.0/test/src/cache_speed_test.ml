open! Core
open! Async
module Test_cache = Test_cache.Test_cache

let main ~iterations ~duration ~cache_slots ~close_idle_resources_when_at_limit =
  let config =
    { Resource_cache.Config.max_resources = cache_slots
    ; idle_cleanup_after = Time_ns.Span.day
    ; max_resources_per_id = cache_slots
    ; max_resource_reuse = iterations
    ; close_idle_resources_when_at_limit
    }
  in
  let t = Test_cache.init ~config () in
  let completed = ref 0 in
  let done_ = Ivar.create () in
  let f _ =
    let%map () = Clock_ns.after duration in
    incr completed;
    if !completed = iterations then Ivar.fill done_ (Time.now ())
  in
  let start = Time.now () in
  for _ = 0 to iterations - 1 do
    don't_wait_for (Test_cache.with_any t [ 0 ] ~f >>| ok_exn >>| ignore)
  done;
  let%map end_ = Ivar.read done_ in
  Time.diff end_ start
;;

let command =
  let open Command.Let_syntax in
  Command.async
    ~summary:""
    [%map_open
      let iterations =
        flag
          "-num-jobs"
          (optional_with_default 10000 int)
          ~doc:"NUM number of jobs to run through cache (default 10000)"
      and duration =
        let%map duration =
          flag
            "-duration"
            (optional_with_default (sec 0.01) Time.Span.arg_type)
            ~doc:"SPAN length of each job (default (0.01 sec))"
        in
        Time_ns.Span.of_span_float_round_nearest duration
      and cache_slots =
        flag
          "-slots"
          (optional_with_default 10 int)
          ~doc:"NUM number of cache slots (default 10)"
      and close_idle_resources_when_at_limit =
        flag
          "-close-idle-resources-when-at-limit"
          no_arg
          ~doc:" pass through this value to the cache config"
      in
      fun () ->
        let open Deferred.Let_syntax in
        let lower_bound_runtime =
          Int.to_float ((iterations + cache_slots - 1) / cache_slots)
          *. Time_ns.Span.to_sec duration
        in
        printf "Lower bound runtime: %f sec\n" lower_bound_runtime;
        let%map time =
          main ~iterations ~duration ~cache_slots ~close_idle_resources_when_at_limit
        in
        printf "Runtime: %f sec\n" (Time.Span.to_sec time)]
;;
