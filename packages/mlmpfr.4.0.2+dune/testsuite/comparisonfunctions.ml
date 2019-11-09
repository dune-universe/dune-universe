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
  printf "%d\n" (M.cmp op1 op2);
  printf "%d\n" (M.cmp_int op1 (M.get_int op2));
  printf "%d\n" (M.cmp_float op1 (M.get_float op2));
  printf "%d\n" (M.cmp_int_2exp op1 (M.get_int op2) (M.get_int op2));
  printf "%d\n" (M.cmpabs op1 op2);
  printf "%b\n" (M.nan_p op1);
  printf "%b\n" (M.inf_p op1);
  printf "%b\n" (M.number_p op1);
  printf "%b\n" (M.zero_p op1);
  printf "%b\n" (M.regular_p op1);
  printf "%s\n" (match (M.sgn op1) with M.Positive -> "Positive" | M.Negative -> "Negative");
  printf "%b\n" (M.greater_p op1 op2);
  printf "%b\n" (M.greaterequal_p op1 op2);
  printf "%b\n" (M.less_p op1 op2);
  printf "%b\n" (M.lessequal_p op1 op2);
  printf "%b\n" (M.equal_p op1 op2);
  printf "%b\n" (M.lessgreater_p op1 op2);
  printf "%b\n" (M.unordered_p op1 op2)

let _ =
  all (M.make_from_float (1. /. 3.)) (M.make_from_float (1. /. 10.));
  all (M.make_from_float (876.543 /. 3.2)) (M.make_from_float (123.456 /. 2.1));
  all (M.make_from_float (~-. 876.543 /. 3.2)) (M.make_from_float (123.456 /. 2.1));
  all (M.make_from_float (~-. 876.543 /. 3.2)) (M.make_from_float (~-. 123.456 /. 2.1));
  all (M.make_from_float (876.543 /. 3.2)) (M.make_from_float (~-. 123.456 /. 2.1));
  all (M.make_inf M.Positive) (M.make_from_float (~-. 123.456 /. 2.1));
  all (M.make_nan ()) (M.make_from_float (~-. 123.456 /. 2.1));
  Gc.full_major (); (* garbage collector full major *)
