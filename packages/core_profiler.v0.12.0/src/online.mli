(** Use this module for online tracking of perf metrics. When using this module, the
    metrics are written out to stdout every second. There is no mertric file generated for
    offline analysis. The rate at which metrics are dumped to stdout is controlled by the
    environment variable [CORE_PROFILER=PRINT_INTERVAL=N] where [N] is the integer number
    of seconds to wait between outputs.

    [Profiler.don't_check_env] is a no-op, provided for compatibility with [Offline]'s
    interface. [Time_stamp_counter] calibration is liable to happen occasionally when
    calling [at].  [safe_to_delay] checks if it will be necessary soon, and does it in
    advance.  If possible, call [safe_to_delay] (fairly regularly) from a time-insensitive
    point in code (or at least, outside any deltas / groups) to reduce the number of
    spurious jumps in time deltas.

    This is how much the online probes cost. I don't know how much I trust these metrics,
    but here they are:

   + ./inline_benchmarks_runner -q 3 -mat online
   ┌─────────────────────────────────────────────────────┬──────────┬────────────┐
   │ Name                                                │ Time/Run │ Percentage │
   ├─────────────────────────────────────────────────────┼──────────┼────────────┤
   │ [online.ml:Fstats:Fstats] update_in_place           │  12.96ns │      6.94% │
   │ [online.ml:Timer] at                                │  11.56ns │      6.19% │
   │ [online.ml:Timer] group_point_at (0 sources)        │  13.31ns │      7.13% │
   │ [online.ml:Timer] group_point_at (1 sources)        │  22.91ns │     12.27% │
   │ [online.ml:Timer] group_point_at (2 sources)        │  40.45ns │     21.66% │
   │ [online.ml:Timer] group_reset                       │  11.58ns │      6.20% │
   │ [online.ml:Probe] at                                │  17.94ns │      9.61% │
   │ [online.ml:Probe] group_point_at (0 sources)        │  12.03ns │      6.44% │
   │ [online.ml:Probe] group_point_at (1 sources)        │  21.35ns │     11.43% │
   │ [online.ml:Probe] group_point_at (2 sources)        │  32.17ns │     17.22% │
   │ [online.ml:Probe] group_reset                       │  11.32ns │      6.06% │
   │ [online.ml:Delta_timer] start_async                 │  11.52ns │      6.17% │
   │ [online.ml:Delta_timer] stop_async                  │  21.99ns │     11.77% │
   │ [online.ml:Delta_timer] start                       │  11.63ns │      6.23% │
   │ [online.ml:Delta_timer] stop                        │  21.99ns │     11.77% │
   │ [online.ml:Delta_timer.wrap_sync] nop               │   2.87ns │      1.54% │
   │ [online.ml:Delta_timer.wrap_sync] wrapped_nop       │  45.51ns │     24.37% │
   │ [online.ml:Delta_timer.wrap_sync] count_256         │ 146.13ns │     78.25% │
   │ [online.ml:Delta_timer.wrap_sync] wrapped_count_256 │ 186.75ns │    100.00% │
   │ [online.ml:Delta_probe] start                       │   3.16ns │      1.69% │
   │ [online.ml:Delta_probe] stop                        │  17.59ns │      9.42% │
   │ [online.ml:Delta_probe] start_async                 │   2.96ns │      1.58% │
   │ [online.ml:Delta_probe] stop_async                  │  17.26ns │      9.24% │
   └─────────────────────────────────────────────────────┴──────────┴────────────┘
*)

open! Core

include (Core_profiler_disabled.Intf.Profiler_intf
         with type Delta_timer.state = private Time_ns.t
          and type Delta_probe.state = private int)
