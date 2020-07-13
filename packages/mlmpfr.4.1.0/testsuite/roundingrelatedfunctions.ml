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

let all op p r1 r2 err =
  M.set_default_rounding_mode M.To_Nearest;
  printf "%s\n" (M.print_rnd_mode (M.get_default_rounding_mode ()));
  M.set_default_rounding_mode r1;
  printf "%s\n" (M.print_rnd_mode (M.get_default_rounding_mode ()));
  printf "%s\n" (M.get_formatted_str (M.prec_round op p));
  printf "%s\n" (M.print_rnd_mode (M.get_default_rounding_mode ()));
  printf "%b\n" (M.can_round op err r1 r2 p);
  printf "%d\n" (M.min_prec op)

let _ =
  all (M.make_from_str ~base:2 "1.101010111001001001100e17") 40 M.Toward_Minus_Infinity M.Toward_Plus_Infinity 1;
  all (M.make_from_float ~rnd:M.To_Nearest (1. /. 3.)) 80 M.Toward_Minus_Infinity M.Toward_Zero 400;
  all (M.make_from_float (12345.78 /. 3.)) 800 M.Away_From_Zero M.Toward_Zero 400;
  all (M.make_from_float (12345.78 /. 3.)) 800 M.Faithful M.Toward_Zero 400;
  all (M.make_nan ()) 80 M.Away_From_Zero M.Toward_Zero 40;
  all (M.make_inf M.Positive) 80 M.Away_From_Zero M.Toward_Zero 40;
  all (M.make_inf M.Negative) 80 M.Away_From_Zero M.Toward_Zero 40;
  Gc.full_major (); (* garbage collector full major *)
