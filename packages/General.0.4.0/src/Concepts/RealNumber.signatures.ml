
module type S0 = sig
  type t

  module O: Operators.S0 with type t := t

  include Number.S0 with type t := t and module O := O
  include Traits.Comparable.S0 with type t := t and module O := O

  val abs: t -> t
  val modulo: t -> t -> t

  (* @feature Traits.ToStandardNumbers? *)
  val to_int: t -> int
  val to_float: t -> float

  (* @feature sign *)
end
