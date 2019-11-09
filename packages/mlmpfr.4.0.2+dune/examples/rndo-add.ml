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

(* Direct translation of mpfr's examples/rndo-add.c,
   local build with: ocamlc -I ../_build/src -custom -o rndo-add \
     ../_build/src/mpfr.cmo rndo-add.ml ../_build/src/mlmpfr_stubs.o \
      -ccopt -L/usr/local/lib -cclib -lmpfr -w -24 *)

open Printf
module M = Mpfr

let () =
  if Array.length Sys.argv <> 4 then begin
    fprintf stderr "Usage: rndo-add <prec> <x> <y>\n";
    exit 1
  end;

  let prec = int_of_string Sys.argv.(1) in
  if prec < 2 then begin
    fprintf stderr "rndo-add: bad precision\n";
    exit 1
  end;
  let pprec = prec - 1 in

  (* Note: make_from_str uses mpfr_strtofr. We can't check that x and y values
     are valid as in the original C source file since M.set_str isn't
     implemented. FIXME *)
  let x = M.make_from_str Sys.argv.(2) in
  let y = M.make_from_str Sys.argv.(3) in
  (* mlmpfr doesn't provide directt binding for mpfr_printf, but similar output
     is obtained with get_formatted_str. *)
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 x);
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 y);

  let d = M.add x y ~rnd:M.Toward_Minus_Infinity in
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 d);

  let u = M.add x y ~rnd:M.Toward_Plus_Infinity in
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 u);

  let e = M.div_2int (M.add d u) 1 in
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 e);

  let z = M.add (M.sub u e) d in
  printf "%s\n" (M.get_formatted_str ~size:pprec ~base:2 z)
