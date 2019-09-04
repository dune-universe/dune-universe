module type S0 = sig
  type t

  include Number.Operators.S0 with type t := t
  include Traits.Comparable.Operators.S0 with type t := t

  val (mod): t -> t -> t
end
