(** Module for the type [unit].  This is mostly useful for building functor arguments. *)

open! Import

type t = unit [@@deriving typerep]

include module type of struct include Base.Unit end
  with type t := t

include Identifiable.S
  with type t := t
   and type comparator_witness := comparator_witness

include Quickcheckable.S with type t := t

module type S = sig end

type m = (module S)
