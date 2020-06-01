open! Base
open! Import

type t = Sign.t =
  | Neg
  | Zero
  | Pos
[@@deriving accessors]
