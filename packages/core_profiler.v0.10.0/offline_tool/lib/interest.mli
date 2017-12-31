open! Core
open Core_profiler
open Core_profiler_disabled

module Interval_subject : sig
  type t = Value | Delta | Time_delta [@@deriving sexp, compare]

  val of_string : string -> t
  val to_string : t      -> string
  val to_int    : t      -> int
end

(** A raw or "unfiltered" interest.
    Specifically, an [Event_generator.t] can produce events given a buffer and a list of
    raw interests, which are typically then "filtered" by a [Filter.t] (a filter
    takes a list of actual interests / [Interest.t]s).
    An event will only ever match one [Interest.Raw.t] (indeed, events are keyed by
    raw interests in [Event_generator.t]) whereas it could match several distinct
    [Interest.t]s (e.g., overlapping intervals) *)
module Raw : sig
  type 'a t =
    | Single of 'a
    | Group_point of 'a * 'a
    | Group_path of 'a * 'a Path.t
  [@@deriving sexp, compare]

  module I : sig
    type id_raw_interest = Probe_id.t t
    type t = id_raw_interest
    include Comparable.S with type t := t
    include Hashable  .S with type t := t
  end
end

(** A [t] specifies some subset of the core-profiler file that we're interested in. *)
type 'a t =
  | All of 'a Raw.t
  | In_interval of 'a Raw.t * Interval_subject.t * Profiler_units.t * Interval.Int.t
[@@deriving sexp, compare]

(** When keyed by [Probe_id.t]s, interests are comparable, hashable. *)
module I : sig
  type id_interest = Probe_id.t t
  type t = id_interest
  include Comparable.S with type t := t
  include Hashable  .S with type t := t
end

(** If this is a filtered interest, this drills down to the 'raw'
    unfiltered interest. Currently, this means it retrieves the
    first argument of an [In_interval] interest, and is the identity
    function otherwise. *)
val raw : 'a t -> 'a Raw.t

(** In the presence of special characters,
    [Fn.compose string_t_of_string string_t_to_string] might not be the identify
    function; indeed, it may even raise an error. *)
val string_t_of_sexp : Sexp.t -> string t
val sexp_of_string_t : string t -> Sexp.t
val string_t_of_string : string -> string t
val string_t_to_string : string t -> string

val lookup_ids : string t -> Util.Name_map.t -> Probe_id.t t
val lookup_names : Probe_id.t t -> Reader.Header.t -> string t
val id_t_to_string : Probe_id.t t -> Reader.Header.t -> string

(** Retrieve the [Probe_type.t] associated with this interest, by drilling down to
    the relevant [Probe_id.t] of the group or single *)
val spec : Probe_id.t t -> Reader.Header.t -> Probe_type.t
val is_path : _ t -> bool

(** If necessary, coerce the units of any values in this interest to those that
    the probe that the interest refers to is quoted in. *)
val coerce_interval_units : Probe_id.t t -> Reader.Header.t -> Probe_id.t t

val readme : string Lazy.t

val arg_type : string t Command.Spec.Arg_type.t
val list_arg : string t list Command.Spec.param

(** Generate a (hopefully sane) set of default interests for the items in this header *)
val default_interests : Reader.Header.t -> Probe_id.t t list
