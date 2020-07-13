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

let _ =
  let x = M.make_from_float (1. /. 3.) in
  printf "%e\n" (M.get_float x);
  printf "%d\n" (M.get_int x);
  let m, n = M.get_float_2exp x in
  printf "%e %d\n" m n;
  let _, n = M.get_mpfr_2exp x in
  printf "%d\n" n;
  printf "%d\n" (M.get_str_ndigits 10 100);
  let m, n = M.get_str x in
  printf "%s %s\n" m n;
  printf "%s\n" (M.get_formatted_str x);
  let stream = open_out "fpif_export.output" in
  M.fpif_export stream x;
  close_out stream;
  let stream = open_in Sys.argv.(1) in
  let m = M.fpif_import stream in
  printf "%s\n" (M.get_formatted_str m);
  close_in stream;
  printf "%b\n" (M.fits_int_p x);
  Gc.full_major (); (* garbage collector full major *)
