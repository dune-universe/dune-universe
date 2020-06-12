open! Core_kernel
open! Import

include sig
  type t =
    | Nanosecond
    | Microsecond
    | Millisecond
    | Second
    | Minute
    | Hour
    | Day
  [@@deriving accessors]
end
with type t := Unit_of_time.t
