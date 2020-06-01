open! Base
open! Import

include sig
  type 'a t =
    | Incl of 'a
    | Excl of 'a
    | Unbounded
  [@@deriving accessors]
end
with type 'a t := 'a Maybe_bound.t
