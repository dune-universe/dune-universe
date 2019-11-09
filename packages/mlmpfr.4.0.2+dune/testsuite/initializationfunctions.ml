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

let _ =
  M.set_default_prec 12345;
  printf "default_precision: %d\n" (M.get_default_prec ());
  let one = M.make_from_int 1 in
  printf "%s\n" (rounding_to_string one);
  let x = M.make_from_mpfr one in
  printf "%s\n" (rounding_to_string x);
  let x = M.make_from_float 1. in
  printf "%s\n" (rounding_to_string x);
  let x = M.make_from_str "1" in
  printf "%s\n" (rounding_to_string x);
  let x = M.make_nan () in
  printf "%s\n" (rounding_to_string x);
  let x = M.make_inf M.Positive in
  printf "%s\n" (rounding_to_string x);
  let x = M.make_zero M.Negative in
  printf "%s\n" (rounding_to_string x);
  printf "%d\n" (M.get_prec x);
  Gc.full_major (); (* garbage collector full major *)
