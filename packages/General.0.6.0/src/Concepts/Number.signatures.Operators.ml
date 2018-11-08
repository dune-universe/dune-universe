module type S0 = sig
  type t

  include Traits.Equatable.Operators.S0 with type t := t
  include Traits.Ringoid.Operators.S0 with type t := t
end
