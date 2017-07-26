open! Core
open Core_profiler

type -'rw t

val create : Reader.Header.t -> [< _ perms] t
val read_only : [> read] t -> read t

val add_interest : read_write t -> Probe_id.t Interest.t -> unit

(** Which interests does this event match? *)
val test : [> read] t -> Event_generator.event -> Probe_id.t Interest.t list

(** Does this event match anything? *)
val test' : [> read] t -> Event_generator.event -> bool

(** [f] is given the event, and the interests it matched *)
val iter_events :
  [> read] t
  -> Event_generator.t
  -> f:(Event_generator.event -> Probe_id.t Interest.t list -> unit)
  -> unit

val iter_events_interests :
  [> read] t
  -> Event_generator.t
  -> f:(Event_generator.event -> Probe_id.t Interest.t -> unit)
  -> unit

(** All raw interests the filter is (currently) interested in receiving events for.
    In the basic case, you'll want to construct a [t] (a filter), and then build a
    [Event_generator.t] using the raw interests specified by this function. *)
val raw_interests : [> read] t -> Probe_id.t Interest.Raw.t list
