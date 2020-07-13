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
module M = Mpfr

let all op1 op2 =
  let r = M.add op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.add_int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.add_float op1 (M.get_float op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.sub op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.sub_int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.int_sub (M.get_int op1) op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.sub_float op1 (M.get_float op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.float_sub (M.get_float op1) op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.mul op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.mul_int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.mul_float op1 (M.get_float op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.div op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.div_int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.int_div (M.get_int op1) op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.div_float op1 (M.get_float op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.float_div (M.get_float op1) op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.sqrt op1 in printf "%s\n" (M.get_formatted_str r);
  let r =
    try
      M.sqrt_int (M.get_int op1)
    with M.Invalid_integer_input _ ->
      M.make_nan ()
  in printf "%s\n" (M.get_formatted_str r);
  let r = M.cbrt op1 in printf "%s\n" (M.get_formatted_str r);
  let r = M.rootn_int op1 10 in printf "%s\n" (M.get_formatted_str r);
  let r = M.pow op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.pow_int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.neg op1 in printf "%s\n" (M.get_formatted_str r);
  let r = M.abs op1 in printf "%s\n" (M.get_formatted_str r);
  let r = M.dim op1 op2 in printf "%s\n" (M.get_formatted_str r);
  let r = M.mul_2int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r);
  let r = M.div_2int op1 (M.get_int op2) in printf "%s\n" (M.get_formatted_str r)

let _ =
  all (M.make_from_float (1. /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float ((~-. 4.) /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float (1. /. 3.)) (M.make_from_float ((~-. 113.) /. 10.)); printf "\n";
  all (M.make_zero M.Positive) (M.make_from_float ((~-. 1.) /. 10.)); printf "\n";
  all (M.make_zero M.Negative) (M.make_zero M.Negative); printf "\n";
  Gc.full_major (); (* garbage collector full major *)
