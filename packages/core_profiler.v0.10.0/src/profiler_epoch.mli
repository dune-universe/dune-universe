(** [Time_ns] and [Time] represents time since 1970 (the unix epoch). When writing out
    perf mertics, we don't have enough bits to express nanos since the unix epch. Instead
    we record an arbitrary point of time as the [Profiler_epoch.t]. Times can be stored
    with respect to this epoch.

    In the offline protocol, in order to save space in the header, an 'epoch' is written
    to the header, and times are stored as an offset from this epoch.  (See also:
    [Protocol.Short_header]) *)

open! Core

type t [@@deriving sexp]
val of_time : Time_ns.t -> t
val to_time : t -> Time_ns.t

val add    : t -> Time_ns.Span.t -> Time_ns.t
val diff   : t -> Time_ns.t -> Time_ns.Span.t
val of_int : int -> t
val to_int : t -> int
val to_string : t -> string
