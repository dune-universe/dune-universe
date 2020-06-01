open! Base
open! Import

type 'a t = 'a Maybe_bound.t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving accessors]
