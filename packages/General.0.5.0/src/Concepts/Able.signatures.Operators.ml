module type S0 = sig
  type t

  include Identifiable.Operators.S0 with type t := t
  include Traits.Comparable.Operators.S0 with type t := t
end
