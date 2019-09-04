module Substract: sig
  module Make0(M: sig
    type t

    val negate: t -> t
    val add: t -> t -> t
  end): sig
    val substract: M.t -> M.t -> M.t
  end
end

module Square: sig
  module Make0(M: sig
    type t

    val multiply: t -> t -> t
  end): sig
    val square: M.t -> M.t
  end
end

module Exponentiate: sig
  module Make0(M: sig
    type t

    val one: t

    val square: t -> t
    val multiply: t -> t -> t

    val exponentiate_negative_exponent: exponentiate:(t -> int -> t) -> t -> int -> t
  end): sig
    val exponentiate: M.t -> int -> M.t
  end
end
