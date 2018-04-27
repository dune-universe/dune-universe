module type S0 = sig
  type t

  include RealNumber.S0 with type t := t
  include Traits.PredSucc.S0 with type t := t

  (* @feature Bitwise? *)
  (* @feature gcd, lcm, quomod *)
end
