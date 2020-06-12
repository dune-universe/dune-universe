open! Base
open! Import

include sig
  type t =
    | Neg
    | Zero
    | Pos
  [@@deriving accessors]
end
with type t := Sign.t
