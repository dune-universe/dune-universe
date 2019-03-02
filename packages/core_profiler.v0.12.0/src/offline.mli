(** Running a program with offline metrics collection causes it to write out a data file
    on exit. This data file is typically called [profiler.dat]. The collected metrics can
    then be analyzed using [profiler-tool.exe].

    I don't trust these numbers, but here they are:
    {v
    ┌──────────────────────────────────────────────────────┬──────────┬────────────┐
    │ Name                                                 │ Time/Run │ Percentage │
    ├──────────────────────────────────────────────────────┼──────────┼────────────┤
    │ [offline.ml:Timer] at                                │  18.29ns │     10.34% │
    │ [offline.ml:Probe] at                                │  21.92ns │     12.38% │
    │ [offline.ml:Delta_timer] start_async                 │  10.94ns │      6.18% │
    │ [offline.ml:Delta_timer] stop_async                  │  22.78ns │     12.87% │
    │ [offline.ml:Delta_timer] start                       │  11.23ns │      6.34% │
    │ [offline.ml:Delta_timer] stop                        │  22.84ns │     12.91% │
    │ [offline.ml:Delta_timer.wrap_sync] nop               │   2.45ns │      1.38% │
    │ [offline.ml:Delta_timer.wrap_sync] wrapped_nop       │  40.59ns │     22.93% │
    │ [offline.ml:Delta_timer.wrap_sync] count_256         │ 143.57ns │     81.12% │
    │ [offline.ml:Delta_timer.wrap_sync] wrapped_count_256 │ 176.99ns │    100.00% │
    │ [offline.ml:Delta_probe] start                       │   2.47ns │      1.40% │
    │ [offline.ml:Delta_probe] stop                        │  22.11ns │     12.49% │
    │ [offline.ml:Delta_probe] start_async                 │   2.44ns │      1.38% │
    │ [offline.ml:Delta_probe] stop_async                  │  22.20ns │     12.54% │
    └──────────────────────────────────────────────────────┴──────────┴────────────┘
    v}
*)

open! Core

(** In [Offline], a [Delta_probe] differs from a two point [Group] in that for each
    start/stop pair, only one message is written to the buffer. This means that only the
    delta in the probe is available, as opposed to deltas in both probe and time. *)

(** @inline *)
include (Core_profiler_disabled.Intf.Profiler_intf
         with type Timer.t = private int
          and type Probe.t = private int
          and type Delta_timer.state = private Time_ns.t
          and type Delta_probe.state = private int)

