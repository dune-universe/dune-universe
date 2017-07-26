(** Simple cpu usage statistics.

    Samples are calculated as [du/dt] once a second where:

    {v
      du = Used user + system time since last sample
      dt = elapsed "wall clock" time since last sample
    v}

    That is, each sample is the average cpu usage of the program over the last second. *)

open! Core
open! Import

module Sampler : sig
  type t

  val create : unit -> t

  (** Measure the percent of CPU used by this process since the last [take_sample t] call
      (or since [t] was created). *)
  val take_sample : t -> Percent.t
end

(** Returns a pipe of samples as described above, one per second.

    Pushback is not honored on the pipe and the pipe will grow unbounded in memory if it
    is not read from. *)
val samples : unit -> Percent.t Pipe.Reader.t

module Summary : sig
  type t =
    { min : Percent.t
    ; max : Percent.t
    ; avg : Percent.t
    }
  [@@deriving bin_io, sexp]
end

(** Get summarized cpu usage.  Each window is the duration over which the summary should
    be calculated.  The underlying data structure adapts to keep a sufficiently large
    history of samples to calculate summaries for all windows.  The pipe will deliver one
    update per window per second.  Window durations are rounded up to the nearest second.

    Pushback is not honored on the pipe and the pipe will grow unbounded in memory if it
    is not read from. *)
val summaries
  :  windows:Time.Span.t list
  -> (Time.Span.t * Summary.t) Pipe.Reader.t
