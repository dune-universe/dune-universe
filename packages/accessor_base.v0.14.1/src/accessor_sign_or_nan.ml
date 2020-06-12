open! Base
open! Import

type t = Sign_or_nan.t =
  | Neg
  | Zero
  | Pos
  | Nan
[@@deriving accessors]
