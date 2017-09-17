module type S0 = sig
  type t

  module O: Operators.S0 with type t := t

  include Traits.Displayable.S0 with type t := t
  include Traits.Equatable.S0 with type t := t and module O := O
  include Traits.Parsable.S0 with type t := t
  include Traits.Representable.S0 with type t := t
  include Traits.Ringoid.S0 with type t := t and module O := O

  (* @feature Traits.OfStandardNumbers? *)
  (* @feature of_int32, of_int64, of_nativeint *)
  val of_int: int -> t
  val of_float: float -> t
end
