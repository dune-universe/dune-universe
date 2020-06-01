open! Core_kernel
open! Import

type t = Unit_of_time.t =
  | Nanosecond
  | Microsecond
  | Millisecond
  | Second
  | Minute
  | Hour
  | Day
[@@deriving accessors]
