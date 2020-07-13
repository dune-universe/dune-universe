(* This file is part of mlmpfr.

  mlmpfr is free software: you can redistribute it and/or modify it
  under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  mlmpfr is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with mlmpfr. If not, see
  <http://www.gnu.org/licenses/>. *)

open Printf
open Common
module M = Mpfr

let all op1 op2 =
  let r = M.log op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r =
    try
      M.log_int (M.get_int op1)
    with M.Invalid_integer_input _ ->
      M.make_nan ()
  in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.log2 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.log10 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.exp op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.exp2 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.exp10 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.sin op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.cos op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.tan op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r, s = M.sin_cos op1 in printf "%s %s %s\n" (M.get_formatted_str r) (M.get_formatted_str s) (rounding_to_string r);
  let r = M.sec op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.csc op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.cot op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.acos op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.asin op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.atan op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.atan2 op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.sinh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.cosh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.tanh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r, s = M.sinh_cosh op1 in printf "%s %s %s\n" (M.get_formatted_str r) (M.get_formatted_str s) (rounding_to_string r);
  let r = M.sech op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.csch op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.coth op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.acosh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.asinh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.atanh op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r =
    try
      M.fac_int (M.get_int op1)
    with M.Invalid_integer_input _ ->
      M.make_nan ()
  in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.log1p op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.expm1 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.eint op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.eint op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.li2 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.gamma op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.gamma_inc op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.lngamma op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r, s = M.lgamma op1 in printf "%s %s %s\n" (M.get_formatted_str r)
                                    (match s with M.Positive -> "Positive" | M.Negative -> "Negative") (rounding_to_string r);
  let r = M.digamma op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.zeta op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.beta op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.erf op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.erfc op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.j0 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.j1 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.jn (M.get_int (M.abs op2)) op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.y0 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.y1 op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.yn (M.get_int (M.abs op2)) op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.fma op1 op2 r in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.fms op1 op2 r in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.fmma op1 op2 r r in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.fmms op1 op2 r r in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.agm op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.hypot op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.ai op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.const_log2 (M.get_default_prec ()) in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.const_pi (M.get_default_prec ()) in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.const_euler (M.get_default_prec ()) in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.const_catalan (M.get_default_prec ()) in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let rec s ?acc:(acc = []) n =
    if n = 0 then acc
    else s ~acc:(op1::op2::acc) (n-2)
  in
  let l = (s 1000000) in
  let r = M.sum l in
  printf "%s %s %d\n" (M.get_formatted_str r) (rounding_to_string r) (List.length l);
  let m = (s 100000) in
  let r = M.dot l m in
  printf "%s %s %d\n" (M.get_formatted_str r) (rounding_to_string r) (List.length m)

let _ =
  all (M.make_from_float (1. /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float ((~-. 4.) /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float (1. /. 3.)) (M.make_from_float ((~-. 113.) /. 10.)); printf "\n";
  all (M.make_zero M.Positive) (M.make_from_float ((~-. 1.) /. 10.)); printf "\n";
  all (M.make_zero M.Negative) (M.make_zero M.Negative);
  Gc.full_major (); (* garbage collector full major *)
