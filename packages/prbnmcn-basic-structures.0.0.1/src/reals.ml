module Float : Basic_intf.Reals with type t = float = struct
  include Float

  let pp = Format.pp_print_float

  let of_int = float_of_int

  let lebesgue rng_state = Random.State.float rng_state 1.0 [@@inline]

  let npow x n = x ** of_int n

  let neg x = ~-.x [@@inline]

  let ( / ) = ( /. )

  let ( * ) = ( *. )

  let ( - ) = ( -. )

  let ( + ) = ( +. )

  let ( < ) (x : float) (y : float) = x < y [@@inline]

  let ( <= ) (x : float) (y : float) = x <= y [@@inline]

  let ( > ) (x : float) (y : float) = x > y [@@inline]

  let ( >= ) (x : float) (y : float) = x >= y [@@inline]

  let ( = ) (x : float) (y : float) = x = y [@@inline]

  let ( <> ) (x : float) (y : float) = x <> y [@@inline]

  let one = 1.0

  let to_float x = x [@@inline]

  let of_float x = x [@@inline]
end

module Rational : Basic_intf.Reals with type t = Q.t = struct
  let lebesgue rng_state = Q.of_float (Random.State.float rng_state 1.0)

  let rec qpow x n =
    if n < 0 then invalid_arg "pow"
    else if n = 0 then Q.one
    else if n = 1 then x
    else
      let p = qpow x (n / 2) in
      if n mod 2 = 0 then Q.mul p p else Q.(mul x (mul p p))

  let npow x n =
    if n = 0 then Q.one
    else if n > 0 then qpow x n
    else Q.(div one (qpow x (Int.neg n)))

  include Q

  let hash = Hashtbl.hash

  let pp = Q.pp_print
end
