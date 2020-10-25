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

type scalar = float
type elt =
  {
    min: scalar;
    max: scalar;
  }

type t =
  {
    mutable min: scalar;
    mutable max: scalar;
  }

let is_point i = i.min = i.max
let is_positive i = 0. < i.min
let is_negative i = i.max < 0.
let is_null i = i.min = 0. && i.max = 0.
let is_not_positive i = i.max <= 0.
let is_not_negative i = 0. <= i.min

let create () = { min = Float.nan; max = Float.nan; }

let make_point f = { min = f; max = f; }
let make_float f = make_point f
let make_bounds min max = {min; max}
let make (i : elt) : t = make_bounds i.min i.max


let integer i = make_point (float i)

let get (i : t) : elt =
  {
    min = i.min;
    max = i.max;
  }
let ( !! ) = get

let get_min i = i.min
let get_max i = i.max
let get_min_max i = (i.min, i.max)

let radius i =
  let min, max = get_min_max i in
  (max -. min) /. 2.

let print2d x y _ =
  let () = Printf.printf "%f\t%f\n" x.min y.min in
  let () = Printf.printf "%f\t%f\n" x.max y.min in
  let () = Printf.printf "%f\t%f\n" x.max y.max in
  let () = Printf.printf "%f\t%f\n" x.min y.max in
  let () = Printf.printf "%f\t%f\n" x.min y.min in
  ()

let to_string i =
  Printf.sprintf "[%f,%f]" i.min i.max
let string_of_scalar = string_of_float
let string_of_elt (i : elt) =
  Printf.sprintf "[%f,%f]" i.min i.max

let copy i = {
    min = i.min;
    max = i.max;
  }
let deepcopy = copy

let zero () = integer 0
let one () = integer 1
let two () = integer 2

let scale i f =
  make { min = i.min *. f; max = i.max *. f; }

let translate i f =
  make { min = i.min +. f; max = i.max +. f; }

let ( ~+ ) = copy
let ( ~- ) i = make { min = -. i.max; max = -. i.min; }

let ( + ) i1 i2 = {
    min = i1.min +. i2.min;
    max = i1.max +. i2.max;
  }

let ( += ) i1 i2 =
  let () = i1.min <- i1.min +. i2.min in
  let () = i1.max <- i1.max +. i2.max in
  i1

let ( - ) i1 i2 = {
    min = i1.min -. i2.max;
    max = i1.max -. i2.min;
  }

let ( -= ) i1 i2 =
  let () = i1.min <- i1.min -. i2.max in
  let () = i1.max <- i1.max -. i2.min in
  i1

let ( * ) i1 i2 =
  let a = i1.min *. i2.min in
  let b = i1.min *. i2.max in
  let c = i1.max *. i2.min in
  let d = i1.max *. i2.max in
  {
    min = min (min a b) (min c d);
    max = max (max a b) (max c d);
  }

let ( *= ) i1 i2 =
  let i = i1 * i2 in
  let () = i1.min <- i.min in
  let () = i1.max <- i.max in
  i1

let inv i =
  match i.min = 0., i.max = 0. with
  | true, true -> begin
      Printf.eprintf "Interval: zero division";
      exit 1
    end
  | true, false -> {
      min = 1. /. i.max;
      max = Float.infinity;
    }
  | false, true -> {
      min = Float.neg_infinity;
      max = 1. /. i.min;
    }
  | false, false -> begin
      if i.min <= 0. && 0. <= i.max then
        {
          min = Float.neg_infinity;
          max = Float.infinity;
        }
      else
        {
          min = 1. /. i.max;
          max = 1. /. i.min;
        }
    end

let ( / ) i1 i2 = i1 * (inv i2)
let ( /= ) i1 i2 =
  let i = i1 / i2 in
  let () = i1.min <- i.min in
  let () = i1.max <- i.max in
  i1

let sqr i =
  if i.max <= 0. then
    {
      min = i.max *. i.max;
      max = i.min *. i.min;
    }
  else if 0. <= i.min then
    {
      min = i.min *. i.min;
      max = i.max *. i.max;
    }
  else (* i.min < 0. && 0. < i.max *)
    {
      min = 0.;
      max = max (i.min *. i.min) (i.max *. i.max);
    }

let sqrt i =
  {
    min = sqrt i.min;
    max = sqrt i.max;
  }

let log i =
  if not (is_not_negative i) then begin
      Printf.eprintf
        "User assertion failed: %s\n"
        "(Interval) log not defined for negative numbers";
      exit 1 end;
  {
    min = log i.min;
    max = log i.max;
  }

let exp i =
  {
    min = exp i.min;
    max = exp i.max;
  }

let pow_int i n =
  let pmin = Stdlib.(i.min ** (float n)) in
  let pmax = Stdlib.(i.max ** (float n)) in
  if n mod 2 = 0 then
    if 0. <= i.min then
      {
        min = pmin;
        max  = pmax;
      }
    else if i.max <= 0. then
      {
        min = pmax;
        max = pmin;
      }
    else
      {
        min = 0.;
        max = max pmin pmax;
      }
  else
    {
      min = pmin;
      max = pmax;
    }

let ( ** ) i1 i2 =
  exp (i2 * (log i1))

let sin i =
  assert false (* TODO *)

let cos i =
  assert false (* TODO *)

let tan i =
  assert false (* TODO *)

let asin i =
  assert false (* TODO *)

let acos i =
  assert false (* TODO *)

let atan i =
  assert false (* TODO *)

let ( = ) i1 i2 =
  i1.min = i2.min && i1.max = i2.max

let ( <> ) i1 i2 =
  i1.min <> i2.min || i1.max <> i2.max

let ( < ) i1 i2 =
  i1.max < i2.min

let ( <= ) i1 i2 =
  i1.max <= i2.min

let ( > ) i1 i2 =
  i1.min > i2.max

let ( >= ) i1 i2 =
  i1.min >= i2.max

(** [subset s1 s2] tests whether the set s1 is a subset of the set s2. *)
let subset i1 i2 =
  Stdlib.(i2.min <= i1.min && i1.max <= i2.max)

let add = ( + )
let sub = ( - )
let mul = ( * )
let div = ( / )
let neg = ( ~- )
