type t = LT | EQ | GT

let of_standard compare =
  fun x y ->
    match compare x y with
      | 0 -> EQ
      | c when OCSP.(<) c 0 -> LT
      | _ -> GT

module Poly = struct
  let compare x y = of_standard OCSP.compare x y
  let less_than = OCSP.(<)
  let less_or_equal = OCSP.(<=)
  let greater_or_equal = OCSP.(>=)
  let greater_than = OCSP.(>)

  let between x ~low ~high =
    OCSP.(&&) (less_than low x) (greater_than high x)

  let between_or_equal x ~low ~high =
    OCSP.(&&) (less_or_equal low x) (greater_or_equal high x)

  let min = OCSP.min
  let max = OCSP.max

  let min_max x y =
    match compare x y with LT -> (x, y) | GT | EQ -> (y, x)

  module O = struct
    let (<) = OCSP.(<)
    let (<=) = OCSP.(<=)
    let (>=) = OCSP.(>=)
    let (>) = OCSP.(>)
  end
end
