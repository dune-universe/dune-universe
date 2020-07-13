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
  let r = M.rint op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.ceil op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.floor op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.round op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.roundeven op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.trunc op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.rint_ceil op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.rint_floor op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.rint_round op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.rint_roundeven op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.rint_trunc op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r = M.frac op1 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r1, r2 = M.modf op1 in printf "%s %s %s %s\n" (M.get_formatted_str r1) (rounding_to_string r1)
                                                    (M.get_formatted_str r2) (rounding_to_string r2);
  let r = M.fmod op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r, q = M.fmodquo op1 op2 in printf "%s %s %d\n" (M.get_formatted_str r) (rounding_to_string r) q;
  let r = M.remainder op1 op2 in printf "%s %s\n" (M.get_formatted_str r) (rounding_to_string r);
  let r, q = M.remquo op1 op2 in printf "%s %s %d\n" (M.get_formatted_str r) (rounding_to_string r) q;
  let r = M.integer_p op1 in printf "%b\n" r

let _ =
  all (M.make_from_float (1. /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float ((~-. 4.) /. 3.)) (M.make_from_float (1. /. 10.)); printf "\n";
  all (M.make_from_float (1. /. 3.)) (M.make_from_float ((~-. 113.) /. 10.)); printf "\n";
  all (M.make_zero M.Positive) (M.make_from_float ((~-. 1.) /. 10.)); printf "\n";
  all (M.make_zero M.Negative) (M.make_zero M.Negative);
  Gc.full_major (); (* garbage collector full major *)
