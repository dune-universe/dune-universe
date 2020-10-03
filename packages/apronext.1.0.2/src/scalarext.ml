open Apron

include Scalar

let to_mpqf = function
  | Mpqf x -> x
  | Float x -> Mpqf.of_float x
  | Mpfrf x -> Mpfrf.to_mpqf x

let to_float = function
  | Mpqf x -> Mpqf.to_float x
  | Float x -> x
  | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x

(** scalar addition. result is automatically lifted to mpqf to avoid
   loss of precision *)
let add s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.add x1 x2)

(** scalar substraction. result is automatically lifted to mpqf to avoid
   loss of precision *)
let sub s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.sub x1 x2)

(** scalar multiplication. result is automatically lifted to mpqf to
   avoid loss of precision *)
let mul s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.mul x1 x2)

(** scalar division. result is automatically lifted to mpqf to avoid
   loss of precision *)
let div s1 s2 =
  let x1 = to_mpqf s1 and x2 = to_mpqf s2 in
  Mpqf (Mpqf.div x1 x2)
