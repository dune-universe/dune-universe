open! Core
open Core_profiler_disabled

type t =
  | Timer
  | Probe of Profiler_units.t
[@@deriving sexp, compare]

let to_string = function
  | Timer -> "Timer"
  | Probe u -> "Probe " ^ (Profiler_units.to_string u)

let to_char = function
  | Timer              -> 'M'
  | Probe Words        -> 'W'
  | Probe Seconds      -> 'S'
  | Probe Nanoseconds  -> 'N'
  | Probe Int        -> 'O'

let of_char = function
  | 'M' -> Timer
  | 'W' -> Probe Words
  | 'S' -> Probe Seconds
  | 'N' -> Probe Nanoseconds
  | 'O' -> Probe Int
  |  _  -> failwith "Invalid Spec character"

let is_probe = function
  | Timer -> false
  | Probe _ -> true

let units = function
  | Timer -> None
  | Probe u -> Some u

