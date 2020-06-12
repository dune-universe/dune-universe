open! Base
open! Import

type t = Ordering.t =
  | Less
  | Equal
  | Greater
[@@deriving accessors]
