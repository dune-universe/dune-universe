open! Core
open Core_profiler
open Core_profiler_disabled

(** A [Name_map.t] maps string names to [Core_profiler_stubs.Common.Probe_id.t]s *)
module Name_map : sig
  type group = { id : Probe_id.t; children : Probe_id.t String.Map.t }
  type t = { singles : Probe_id.t String.Map.t; groups : group String.Map.t }
  val of_id_map : Reader.Header.t -> t
end

(* See Core_profiler_stubs.Common for the counterpart int_units_to_string *)
val int_units_of_string : string -> int * Profiler_units.t
val span_to_string : Time_ns.Span.t -> string

val coerce_units : int -> current:Profiler_units.t -> desired:Profiler_units.t -> int

(** Choose the best 'common' units to use to coerce the two arguments to *)
val choose_units : Profiler_units.t -> Profiler_units.t -> Profiler_units.t

val time_ns_to_ofday_string : Time_ns.t -> string

val choices_argtype : string -> (string * 'a) list -> 'a Command.Spec.Arg_type.t
