open! Core_kernel
open! Import

type t = Month.t =
  | Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
[@@deriving accessors]

let shifted i =
  Accessor.isomorphism
    ~get:(fun month -> Month.shift month i)
    ~construct:(fun month -> Month.shift month (-i))
;;
