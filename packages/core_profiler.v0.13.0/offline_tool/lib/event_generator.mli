open! Core
open Core_profiler

(** Reads profiler data stored in a file and filters them appropriately *)
type t

type timer_path =
  { interest: Probe_id.t Interest.Raw.t
  ; time : Time_ns.t
  ; time_delta : Time_ns.Span.t
  }
[@@deriving sexp, compare]

type probe_path =
  { interest: Probe_id.t Interest.Raw.t
  ; time : Time_ns.t
  ; time_delta : Time_ns.Span.t
  ; value : int
  ; delta : int
  }
[@@deriving sexp, compare]

type event =
  (* Timer and probe events are emitted for both singles and points. *)
  | Timer      of Probe_id.t Interest.Raw.t * Time_ns.t
  | Probe      of Probe_id.t Interest.Raw.t * Time_ns.t * int
  | Timer_path of timer_path
  | Probe_path of probe_path
[@@deriving sexp, compare]

val event_time : event -> Time_ns.t

(** A [t] emits (via [iter_events]) all [event]s that match the list of
    raw interests provided. The source of the event is identified
    by passing the raw interest inside the [event]; interests are hashable
    and comparable. *)
val create :
  Profiler_epoch.t
  -> Reader.Header.t
  -> Probe_id.t Interest.Raw.t list
  -> ([> read ], _) Iobuf.t
  -> t

val iter_events : t -> f:(event -> unit) -> unit
