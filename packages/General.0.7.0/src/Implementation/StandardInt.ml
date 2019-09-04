module Make(M: sig
  type t

  val name: string
  val repr_suffix: string

  val zero: t
  val one: t
  (* val minus_one: t *)
  val neg: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div: t -> t -> t
  val rem: t -> t -> t
  val succ: t -> t
  val pred: t -> t
  val abs: t -> t
  val max_int: t
  val min_int: t
  (* val logand: t -> t -> t *)
  (* val logor: t -> t -> t *)
  (* val logxor: t -> t -> t *)
  (* val lognot: t -> t *)
  (* val shift_left: t -> int -> t *)
  (* val shift_right: t -> int -> t *)
  (* val shift_right_logical: t -> int -> t *)
  val of_int: int -> t
  val to_int: t -> int
  val of_float: float -> t
  val to_float: t -> float
  val of_string: string -> t
  val to_string: t -> string
  val compare: t -> t -> int
  val equal: t -> t -> bool
end): sig
  type t = M.t

  include Concepts.Integer.S0 with type t := t

  val smallest: t
  val greatest: t
end = struct
  module SelfA = struct
    open M

    type nonrec t = t

    let zero = zero
    let one = one
    let greatest = max_int
    let smallest = min_int

    let of_float = of_float
    let to_float = to_float
    let of_int = of_int
    let to_int = to_int
    let of_string = of_string
    let try_of_string s =
      Exception.or_none (lazy (of_string s))
    let to_string = to_string
    let repr n =
      Format.apply "%s%s" (to_string n) repr_suffix

    let abs = abs

    let succ = succ
    let pred = pred

    let negate = neg
    let add = add
    let substract = sub
    let multiply = mul
    let divide = div
    let modulo = rem

    let compare = Compare.of_standard compare
    let equal = equal
  end

  module SelfB = struct
    include Traits.Comparable.GreaterLessThan.Make0(SelfA)
    include Traits.Comparable.MinMax.Make0(SelfA)
    include Traits.Equatable.Different.Make0(SelfA)
    include Traits.Ringoid.Square.Make0(SelfA)

    include SelfA
  end

  module SelfC = struct
    include Traits.Ringoid.Exponentiate.Make0(struct
      include SelfB

      let exponentiate_negative_exponent ~exponentiate:_ _ n =
        Exception.invalid_argument "%s.exponentiate: Negative exponent: %i" M.name n
    end)
    include Traits.Comparable.Between.Make0(SelfB)

    include SelfB
  end

  module Self = struct
    module O = struct
      include Traits.Comparable.Operators.Make0(SelfC)
      include Traits.Equatable.Operators.Make0(SelfC)
      include Traits.Ringoid.Operators.Make0(SelfC)

      let (mod) = SelfC.modulo
    end

    include SelfC
  end

  include Self
end
