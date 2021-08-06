let add = ( + )

let rec add_2 x y =
  match (x, y) with (0, y) -> y | (x, y) -> 1 + add_2 (x - 1) y

module Math = struct
  let math_add = ( + )
end

module MathFunct (MATH : sig
  val math_add : int -> int -> int
end) =
struct
  let math_funct_add = MATH.math_add
end
