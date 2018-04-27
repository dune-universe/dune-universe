type t = float

let zero = 0.
let one = 1.
let pi = OCSP.(4. *. atan 1.)
let e = OCSP.exp 1.
let smallest = OCSP.min_float
let greatest = OCSP.max_float
let epsilon = OCSP.epsilon_float
let infinity = OCSP.infinity
let negative_infinity = OCSP.neg_infinity
let not_a_number = OCSP.nan

let of_int = OCSP.float_of_int
let to_int = OCSP.int_of_float
let of_float = Functions.Function1.identity
let to_float = Functions.Function1.identity
let of_string = OCSP.float_of_string
let try_of_string s =
  Exception.or_none (lazy (of_string s))
let to_string = OCSP.string_of_float

let of_parts ~significand ~exponent = OCSP.ldexp significand exponent
let to_parts = OCSP.frexp

let to_fractional_and_integral = OCSP.modf

let repr = OCSP.string_of_float

let add = OCSP.(+.)
let substract = OCSP.(-.)
let negate = OCSP.(~-.)
let posate = OCSP.(~+.)
let multiply = OCSP.( *. )
let divide = OCSP.(/.)
let square x = multiply x x
let abs = OCSP.abs_float
let modulo = OCSP.mod_float

let sqrt = OCSP.sqrt

let exp = OCSP.exp
let log = OCSP.log
let log10 = OCSP.log10
let expm1 = OCSP.expm1
let log1p = OCSP.log1p

let cos = OCSP.cos
let sin = OCSP.sin
let tan = OCSP.tan
let acos = OCSP.acos
let asin = OCSP.asin
let atan = OCSP.atan
let atan2 ~y ~x = OCSP.atan2 y x
let hypot = OCSP.hypot
let cosh = OCSP.cosh
let sinh = OCSP.sinh
let tanh = OCSP.tanh

let ceil = OCSP.ceil
let floor = OCSP.floor
let copy_sign x ~sign = OCSP.copysign x sign

module O = struct
  include Compare.Poly.O
  include Equate.Poly.O

  let (~-) = OCSP.(~-.)
  let (~+) = OCSP.(~+.)
  let (+) = OCSP.(+.)
  let (-) = OCSP.(-.)
  let ( * ) = OCSP.( *. )
  let (/) = OCSP.(/.)
  let (mod) = OCSP.mod_float
end

include (Compare.Poly: module type of Compare.Poly with module O := O)
include (Equate.Poly: module type of Equate.Poly with module O := O)

let approx_equal ?(precision=1e-10) a b =
  less_than (abs (substract a b)) precision

module O_dot = struct
  let (~-.) = OCSP.(~-.)
  let (~+.) = OCSP.(~+.)
  let (+.) = OCSP.(+.)
  let (-.) = OCSP.(-.)
  let ( *. ) = OCSP.( *. )
  let (/.) = OCSP.(/.)
  let ( ** ) = OCSP.( ** )
end

module Class = struct
  type t =
    | Normal
    | SubNormal
    | Zero
    | Infinite
    | NotANumber

  let of_float x =
    match OCSP.classify_float x with
      | OCSP.FP_normal -> Normal
      | OCSP.FP_subnormal -> SubNormal
      | OCSP.FP_zero -> Zero
      | OCSP.FP_infinite -> Infinite
      | OCSP.FP_nan -> NotANumber

  let repr = function
    | Normal -> "Normal"
    | SubNormal -> "SubNormal"
    | Zero -> "Zero"
    | Infinite -> "Infinite"
    | NotANumber -> "NotANumber"

  module O = struct
    include Compare.Poly.O
    include Equate.Poly.O
  end

  include (Compare.Poly: module type of Compare.Poly with module O := O)
  include (Equate.Poly: module type of Equate.Poly with module O := O)
end
