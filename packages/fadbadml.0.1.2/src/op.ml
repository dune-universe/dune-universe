(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module Float =
struct
  type t = float ref

  type elt = float
  type scalar = float

  let create () = ref 7895.

  let make x = ref x
  let get f = !f
  let ( !! ) = get

  let integer i = ref (float i)

  let to_string x = string_of_float !x
  let string_of_scalar = string_of_float
  let string_of_elt = string_of_float

  let copy x = ref !x
  let deepcopy x = copy x

  let zero () = ref 0.
  let one () = ref 1.
  let two () = ref 2.

  let scale x a = ref (a *. !x)
  let translate x a = ref (!x +. a)

  let ( ~+ ) = copy
  let ( ~- ) x = Stdlib.(ref (~-. !x))

  let ( + ) x y = Stdlib.(ref (!x +. !y))
  let ( += ) x y = Stdlib.(x := (!x +. !y)); x

  let ( - ) x y = Stdlib.(ref (!x -. !y))
  let ( -= ) x y = Stdlib.(x := (!x -. !y)); x

  let ( * ) x y = Stdlib.(ref (!x *. !y))
  let ( *= ) x y = Stdlib.(x := (!x *. !y)); x

  let ( / ) x y = Stdlib.(ref (!x /. !y))
  let ( /= ) x y = Stdlib.(x := (!x /. !y)); x

  let ( ** ) x y = Stdlib.(ref (!x ** !y))

  let inv x = Stdlib.(ref (1. /. !x))
  let sqr x = Stdlib.(ref (!x *. !x))

  let sqrt x = Stdlib.(ref (sqrt !x))
  let log x = Stdlib.(ref (log !x))
  let exp x = Stdlib.(ref (exp !x))
  let sin x = Stdlib.(ref (sin !x))
  let cos x = Stdlib.(ref (cos !x))
  let tan x = Stdlib.(ref (tan !x))
  let asin x = Stdlib.(ref (asin !x))
  let acos x = Stdlib.(ref (acos !x))
  let atan x = Stdlib.(ref (atan !x))

  let ( = ) x y = Stdlib.(!x = !y)
  let ( <> ) x y = Stdlib.(!x <> !y)
end

module OrderedFloat =
struct
  include Float
  let ( < ) x y = Stdlib.(get x < get y)
  let ( <= ) x y = Stdlib.(get x <= get y)
  let ( > ) x y = Stdlib.(get x > get y)
  let ( >= ) x y = Stdlib.(get x >= get y)

  let max f1 f2 = make (max (get f1) (get f2))
  let min f1 f2 = make (min (get f1) (get f2))
end
