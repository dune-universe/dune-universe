open! Base
open! Import

include sig
  type t =
    | Neg
    | Zero
    | Pos
    | Nan
  [@@deriving accessors]
end
with type t := Sign_or_nan.t
