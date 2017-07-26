(** Every function in [Disabled] compiles to a no-op with the least overhead
    possible.  Also see comments in [intf.ml]. *)

open! Core

include (Intf.Profiler_intf
         with type Timer.t = private unit
          and type Probe.t = private unit
          and type Delta_timer.t = private unit
          and type Delta_probe.t = private unit)

