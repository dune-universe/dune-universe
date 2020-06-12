open! Core_kernel
open! Import

include sig
  type t =
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
end
with type t := Month.t

(** Access a month shifted by the given number of months. *)
val shifted : int -> (_, Month.t, Month.t, [< isomorphism ]) Accessor.Simple.t
