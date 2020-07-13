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

let all op1 op2 exp =
  printf "%s\n" (M.get_formatted_str (M.nexttoward op1 op2));
  printf "%s\n" (M.get_formatted_str (M.nextabove op1));
  printf "%s\n" (M.get_formatted_str (M.nextbelow op1));
  printf "%s\n" (M.get_formatted_str (M.min op1 op2));
  printf "%s\n" (M.get_formatted_str (M.max op1 op2));
  printf "%d\n" (M.get_exp op1);
  printf "%d\n" (M.get_exp (M.set_exp op1 exp));
  printf "%s\n" (match (M.signbit op1) with M.Positive -> "Positive" | M.Negative -> "Negative");
  printf "%s\n" (M.get_formatted_str (M.copysign op1 op2));
  printf "%s\n" (M.get_version ())

let _ =
  all (M.make_from_float (1. /. 3.)) (M.make_from_float (1. /. 10.)) 321; printf "\n";
  all (M.make_from_float ((~-. 4.) /. 3.)) (M.make_from_float (1. /. 10.)) 3; printf "\n";
  all (M.make_from_float (1. /. 3.)) (M.make_from_float ((~-. 113.) /. 10.)) 89045; printf "\n";
  all (M.make_from_float (0.1)) (M.make_from_float ((~-. 1.) /. 10.)) 808; printf "\n";
  all (M.make_from_float (~-.0.1)) (M.make_from_float (~-.0.1)) 7;
  Gc.full_major (); (* garbage collector full major *)
