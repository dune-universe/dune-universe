type t = bool

let of_string = OCSP.bool_of_string
let try_of_string s =
  Exception.or_none (lazy (of_string s))
let to_string = OCSP.string_of_bool

let repr = OCSP.string_of_bool

let xor x y =
  match (x, y) with
    | (true, false) | (false, true) -> true
    | (true, true) | (false, false) -> false

module O = struct
  include Compare.Poly.O
  include Equate.Poly.O

  let not = OCSP.not
  external (&&): bool -> bool -> bool = "%sequand"
  external (||): bool -> bool -> bool = "%sequor"
  let xor = xor
end

include (Compare.Poly: module type of Compare.Poly with module O:= O)
include (Equate.Poly: module type of Equate.Poly with module O:= O)

let not = OCSP.not
let and_ = OCSP.(&&)
let or_ = OCSP.(||)
