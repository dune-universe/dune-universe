val add : int -> int -> int [@@pbt "commutative; associative[int, int, int]"]

val add_2 : int -> int -> int [@@pbt "commutative"]

module Math : sig
  val math_add : int -> int -> int [@@pbt "commutative"]
end

module MathFunct (MATH : sig
  val math_add : int -> int -> int
end) : sig
  val math_funct_add : int -> int -> int [@@pbt "commutative"]
end
