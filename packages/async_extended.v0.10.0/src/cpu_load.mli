open! Core

(** [start ()] starts measuring CPU load.  It uses an async thread that checks
 * the CPU use every second and maintains stats.
*)
val start : unit -> unit

module Stats : sig
  type t = {
    last_second : float;
    last_minute : float;
    since_start : float;
  }

  include Sexpable with type t := t
end

(** [get ()] returns the most recent stats. *)
val get : unit -> Stats.t

