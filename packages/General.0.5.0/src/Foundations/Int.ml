type t = int

let zero = 0
let one = 1
let smallest = OCSP.min_int
let greatest = OCSP.max_int

let of_int = Functions.Function1.identity
let to_int = Functions.Function1.identity
let of_float = OCSP.int_of_float
let to_float = OCSP.float_of_int
let of_string = OCSP.int_of_string
let try_of_string s =
  Exception.or_none (lazy (of_string s))
let to_string = OCSP.string_of_int

let repr = OCSP.string_of_int

let add = OCSP.(+)
let substract = OCSP.(-)
let negate = OCSP.(~-)
let posate = OCSP.(~+)
let multiply = OCSP.( * )
let divide = OCSP.(/)
let square x = multiply x x
let abs = OCSP.abs
let modulo = OCSP.(mod)
let pred = OCSP.pred
let succ = OCSP.succ

module O = struct
  include Compare.Poly.O
  include Equate.Poly.O

  let (~-) = OCSP.(~-)
  let (~+) = OCSP.(~+)
  let (+) = OCSP.(+)
  let (-) = OCSP.(-)
  let ( * ) = OCSP.( * )
  let (/) = OCSP.(/)
  let (mod) = OCSP.(mod)
end

include (Compare.Poly: module type of Compare.Poly with module O := O)
include (Equate.Poly: module type of Equate.Poly with module O := O)

module Bitwise = struct
  let logical_and = OCSP.(land)
  let logical_or = OCSP.(lor)
  let logical_xor = OCSP.(lxor)
  let logical_not = OCSP.(lnot)
  let logical_shift_left n ~shift = OCSP.(n lsl shift)
  let logical_shift_right n ~shift = OCSP.(n lsr shift)
  let arithmetic_shift_right n ~shift = OCSP.(n asr shift)
end
