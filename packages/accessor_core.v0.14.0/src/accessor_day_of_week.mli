open! Core_kernel
open! Import

include sig
  type t =
    | Sun
    | Mon
    | Tue
    | Wed
    | Thu
    | Fri
    | Sat
  [@@deriving accessors]
end
with type t := Day_of_week.t
