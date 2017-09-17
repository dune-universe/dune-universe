module type S0 = sig
  include Basic.S0

  val square: t -> t
  val exponentiate: t -> int -> t

  module O: Operators.S0 with type t := t
end
