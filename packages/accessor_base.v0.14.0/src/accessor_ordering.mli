open! Base
open! Import

include sig
  type t =
    | Less
    | Equal
    | Greater
  [@@deriving accessors]
end
with type t := Ordering.t
