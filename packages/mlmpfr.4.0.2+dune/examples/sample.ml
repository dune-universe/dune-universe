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

(* Direct translation of https://www.mpfr.org/sample.html,
   local build with: ocamlc -I ../_build/src -custom -o a.out \
     ../_build/src/mpfr.cmo sample.ml ../_build/src/mlmpfr_stubs.o \
      -ccopt -L/usr/local/lib -cclib -lmpfr *)

module M = Mpfr

let main =
  let one = M.make_from_float ~prec:200 ~rnd:M.Toward_Minus_Infinity 1.0 in
  let rec loop sum fact i j =
    let f = M.mul_int ~prec:200 ~rnd:M.Toward_Plus_Infinity fact i in
    let d = M.div ~prec:200 ~rnd:M.Toward_Minus_Infinity one f in
    let s = M.add ~prec:200 ~rnd:M.Toward_Minus_Infinity sum d in
    if i = j then s
    else loop s f (i+1) j
  in
  Printf.printf "Sum is %s\n" (M.get_formatted_str ~rnd:M.Toward_Minus_Infinity
                               ~base:10 ~size:0 (loop one one 1 100));
  M.free_cache
