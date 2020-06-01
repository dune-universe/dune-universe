open! Core
open Core_profiler_disabled

(** Type of a probe ([Intf.S]). *)
type t =
  | Timer
  | Probe of Profiler_units.t
[@@deriving sexp, compare]

val to_string : t -> string
val to_char : t -> char
val of_char : char -> t

val is_probe : t -> bool
val units : t -> Profiler_units.t option
