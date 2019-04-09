(*
    Copyright 2011 Jean-Marc Alliot / Jean-Baptiste Gotteland

    This file is part of the ocaml interval library.

    The ocaml interval library is free software:
    you can redistribute it and/or modify it under the terms of
    the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The ocaml interval library is distributed in the hope that it will be
    useful,but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with the ocaml interval library.
    If not, see <http://www.gnu.org/licenses/>.
*)

(* Utilities for exponentiation *)

external is_neg: float -> bool = "is_neg_caml"

let inf_pow y =
  if y < 0. then 0. else if y = 0. then 1. else infinity

let zero_pow x y =
  if 0. < y then 0.
  else if y = 0. || y = neg_infinity || (is_neg x && floor y <> y) then nan
  else if is_neg x && mod_float y 2. <> 0. then neg_infinity
  else infinity

let neg_inf_pow y =
  if classify_float y = FP_infinite || floor y <> y then nan
  else if y = 0. then 1. else if y < 0. then 0.
  else if mod_float y 2. = 0. then infinity else neg_infinity

let pos_pow_inf x =
  if x < 1. then 0. else if x = 1. then 1. else infinity

let pos_pow_neg_inf x =
  if x < 1. then infinity else if x = 1. then 1. else 0.

external flog_pow: float -> float -> float
  = "flog_pow_caml" "flog_pow" [@@unboxed]
external flog_pow_low: float -> float -> float
  = "flog_pow_low_caml" "flog_pow_low" [@@unboxed]
external flog_pow_high: float -> float -> float
  = "flog_pow_high_caml" "flog_pow_high" [@@unboxed]


(* Operations rounded down. *)
module Low = struct
  include Interval.Low

  external pow: float -> float -> float
    = "flog_pow_low_caml" "flog_pow_low" [@@unboxed]
  external sqrt: float -> float = "fsqrt_low_caml" "fsqrt_low" [@@unboxed]
  external log: float -> float = "flog_low_caml" "flog_low" [@@unboxed]
  external exp: float -> float = "fexp_low_caml" "fexp_low" [@@unboxed]
  external sin: float -> float = "fsin_low_caml" "fsin_low" [@@unboxed]
  external cos: float -> float = "fcos_low_caml" "fcos_low" [@@unboxed]
  external tan: float -> float = "ftan_low_caml" "ftan_low" [@@unboxed]
  external asin: float -> float = "fasin_low_caml" "fasin_low" [@@unboxed]
  external acos: float -> float = "facos_low_caml" "facos_low" [@@unboxed]
  external atan2: float -> float -> float
    = "fatan2_low_caml" "fatan2_low" [@@unboxed]
  external sinh: float -> float = "fsinh_low_caml" "fsinh_low" [@@unboxed]
  external cosh: float -> float = "fcosh_low_caml" "fcosh_low" [@@unboxed]
  external tanh: float -> float = "ftanh_low_caml" "ftanh_low" [@@unboxed]

  let atan x = atan2 x 1.0

  let ( ** ) x y =
    if x = infinity then inf_pow y
    else if 0. < x then
      if y = infinity then pos_pow_inf x
      else if y = neg_infinity then pos_pow_neg_inf x
      else flog_pow_low x y
    else if x = 0. then zero_pow x y
    else if x = neg_infinity then neg_inf_pow y
    else if classify_float y = FP_infinite || floor y <> y then nan
    else if mod_float y 2. = 0. then flog_pow_low (-.x) y
    else -.flog_pow_high (-.x) y
end

(* Operations rounded up. *)
module High = struct
  include Interval.High

  external pow: float -> float -> float
    = "flog_pow_high_caml" "flog_pow_high" [@@unboxed]
  external sqrt: float -> float = "fsqrt_high_caml" "fsqrt_high" [@@unboxed]
  external log: float -> float = "flog_high_caml" "flog_high" [@@unboxed]
  external exp: float -> float = "fexp_high_caml" "fexp_high" [@@unboxed]
  external sin: float -> float = "fsin_high_caml" "fsin_high" [@@unboxed]
  external cos: float -> float = "fcos_high_caml" "fcos_high" [@@unboxed]
  external tan: float -> float = "ftan_high_caml" "ftan_high" [@@unboxed]
  external asin: float -> float = "fasin_high_caml" "fasin_high" [@@unboxed]
  external acos: float -> float = "facos_high_caml" "facos_high" [@@unboxed]
  external atan2: float -> float -> float
    = "fatan2_high_caml" "fatan2_high" [@@unboxed]
  external sinh: float -> float = "fsinh_high_caml" "fsinh_high" [@@unboxed]
  external cosh: float -> float = "fcosh_high_caml" "fcosh_high" [@@unboxed]
  external tanh: float -> float = "ftanh_high_caml" "ftanh_high" [@@unboxed]

  let atan x = atan2 x 1.0

  let ( ** ) x y =
    if x = infinity then inf_pow y
    else if 0. < x then
      if y = infinity then pos_pow_inf x
      else if y = neg_infinity then pos_pow_neg_inf x
      else flog_pow_high x y
    else if x = 0. then zero_pow x y
    else if x = neg_infinity then neg_inf_pow y
    else if classify_float y = FP_infinite || floor y <> y then nan
    else if mod_float y 2. = 0. then flog_pow_high (-.x) y
    else -.flog_pow_low (-.x) y
end

external ffloat: int -> float = "ffloat_caml"
let ffloat_high = High.float
let ffloat_low = Low.float
external fadd: float -> float -> float = "fadd_caml" "fadd" [@@unboxed]
let fadd_low = Low.( +. )
let fadd_high = High.( +. )
external fsub: float -> float -> float = "fsub_caml" "fsub" [@@unboxed]
let fsub_low = Low.( -. )
let fsub_high = High.( -. )
external fmul: float -> float -> float = "fmul_caml" "fmul" [@@unboxed]
let fmul_low = Low.( *. )
let fmul_high = High.( *. )
external fdiv: float -> float -> float = "fdiv_caml" "fdiv" [@@unboxed]
let fdiv_low = Low.( /. )
let fdiv_high = High.( /. )

external fmod: float -> float -> float = "fprem_caml" "fprem" [@@unboxed]

external fsqrt: float -> float = "fsqrt_caml" "fsqrt" [@@unboxed]
let fsqrt_low = Low.sqrt
let fsqrt_high = High.sqrt

external flog: float -> float = "flog_caml" "flog" [@@unboxed]
let flog_low = Low.log
let flog_high = High.log

external fexp: float -> float = "fexp_caml" "fexp" [@@unboxed]
let fexp_low = Low.exp
let fexp_high = High.exp

external fsin: float -> float = "fsin_caml" "fsin" [@@unboxed]
let fsin_low = Low.sin
let fsin_high = High.sin
external fcos: float -> float = "fcos_caml" "fcos" [@@unboxed]
let fcos_low = Low.cos
let fcos_high = High.cos
external ftan: float -> float = "ftan_caml" "ftan" [@@unboxed]
let ftan_low = Low.tan
let ftan_high = High.tan

external fasin: float -> float = "fasin_caml" "fasin" [@@unboxed]
let fasin_low = Low.asin
let fasin_high = High.asin
external facos: float -> float = "facos_caml" "facos" [@@unboxed]
let facos_low = Low.acos
let facos_high = High.acos
external fatan: float -> float -> float = "fatan_caml" "fatan" [@@unboxed]
let fatan_low x y = Low.atan2 y x
let fatan_high x y = High.atan2 y x

external fsinh: float -> float = "fsinh_caml" "fsinh" [@@unboxed]
let fsinh_low = Low.sinh
let fsinh_high = High.sinh
external fcosh: float -> float = "fcosh_caml" "fcosh" [@@unboxed]
let fcosh_low = Low.cosh
let fcosh_high = High.cosh
external ftanh: float -> float = "ftanh_caml" "ftanh" [@@unboxed]
let ftanh_low = Low.tanh
let ftanh_high = High.tanh


let fpow x y =
  if x = infinity then inf_pow y
  else if 0. < x then
    if y = infinity then pos_pow_inf x
    else if y = neg_infinity then pos_pow_neg_inf x
    else flog_pow x y
  else if x = 0. then zero_pow x y
  else if x = neg_infinity then neg_inf_pow y
  else if classify_float y = FP_infinite || floor y <> y then nan
  else if mod_float y 2. = 0. then flog_pow (-.x) y
  else -.flog_pow (-.x) y

let fpow_low = Low.( ** )
let fpow_high = High.( ** )


module Rename = struct
  let mod_float = fmod
  let sqrt = fsqrt
  let log = flog
  let exp = fexp
  let ( ** ) = fpow
  let cos = fcos
  let sin = fsin
  let tan = ftan
  let asin = fasin
  let acos = facos
  let atan x = fatan 1.0 x
  let atan2 y x = fatan x y
  let cosh = fcosh
  let sinh = fsinh
  let tanh = ftanh
end

module Rename_all = struct
  let ( +. ) = fadd
  let ( -. ) = fsub
  let ( *. ) = fmul
  let ( /. ) = fdiv
  let mod_float = fmod
  let sqrt = fsqrt
  let log = flog
  let exp = fexp
  let ( ** ) = fpow
  let cos = fcos
  let sin = fsin
  let tan = ftan
  let asin = fasin
  let acos = facos
  let atan x = fatan 1.0 x
  let atan2 y x = fatan x y
  let cosh = fcosh
  let sinh = fsinh
  let tanh = ftanh
end
