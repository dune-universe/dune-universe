open! Core

type t =
  | Online_profiler
  | Offline_profiler
  | Any_profiler

(** now, possibly with calibration error. *)
val now_no_calibrate : unit -> Time_ns.t

(** [slow_tasks] is a list of functions that should be called rougly every second while
    the library is in use. The time-since-we-last-did-slow-tasks is checked on every call
    to [now] and [maybe_do_slow_tasks] below. [add_slow_task] adds one more slow task. *)
val add_slow_task : t -> (unit -> unit) -> unit

(** [reluctance] is higher if we don't want to do 'slow tasks' / don't want a potential
    300ns spike.

    We _really_ don't want this in [Group.Point.at]s and [Delta_probe.start]s
    (reluctance:4) since the spike would be included in the calculation of a delta.  We'd
    rather not on [Timer.record]s (r:3) since they are more liable to be in the middle of
    something performance sensitive.  We're slightly more happy to calibrate after a
    [Group.reset] (r:2), but ideally want to calibrate on a call to [safe_to_delay] (r:1;
    lowest).

    Here [t] specifies the kind of slow tasks to run if the reluctance has been overcome.
*)
val now : t -> reluctance:int -> unit -> Time_ns.t

val maybe_do_slow_tasks : t -> reluctance:int -> unit
