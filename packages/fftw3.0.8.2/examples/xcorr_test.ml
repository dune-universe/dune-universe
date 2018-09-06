(* File: xcorr_test.ml

   Copyright (C) 2008-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Lacaml.D
open Format
open Xcorr

let () =
  Lacaml.Io.pp_float_el_default := (fun ppf el -> fprintf ppf "%.4g" el)

let pp_fvec ?(nl=true) xname x =
  printf "@[<2>%s = [" xname;
  Lacaml.Io.pp_rfvec std_formatter x;
  printf "]@]";
  if nl then printf "@\n"

let pp_cvec xname x =
  printf "@[<2> %s = [" xname;
  Lacaml.Io.pp_rcvec std_formatter x;
  printf "]@]"

let approx_eq ?(eps=1e-10) a b =
  if Vec.dim a <> Vec.dim b then false else begin
    let eq x y = ((y = 0. && abs_float x <= eps)
                  || abs_float(x -. y) <= eps *. abs_float y) in
    let is_eq = ref true in
    for i = 1 to Vec.dim a do is_eq := !is_eq && eq a.{i} b.{i} done;
    !is_eq
  end

(** [test text a rep] test [a] against the correct result [rep]. *)
let test text a rep =
  printf "%s @[= [" text;
  Lacaml.Io.pp_rfvec std_formatter a;
  if approx_eq a rep then printf "]@] (ok)@\n"
  else (
    printf "]@\n<>[";
    Lacaml.Io.pp_rfvec std_formatter rep;
    printf "]@]@\n"
  )

let () =
  let x = Vec.of_array [| 1.; 2.; 3.; 4. |]
  and y = Vec.of_array [| 1.; 2. |] in
  pp_fvec "x" x;
  pp_fvec "y" y;
  test "xcorr x x" (xcorr x x)
    (Vec.of_array [| 4.; 11.; 20.; 30.; 20.; 11.; 4. |]);
  test "xcorr x x ~scale:Biased" (xcorr x x ~scale:Biased)
    (Vec.of_array [| 1.; 2.75; 5.; 7.5; 5.; 2.75; 1. |]);
  test "xcorr x x ~scale:Unbiased" (xcorr x x ~scale:Unbiased)
    (Vec.of_array [| 4.; 5.5; 20. /. 3.; 7.5; 20. /. 3.; 5.5; 4. |]);
  let rep = Vec.of_array [| 0.4; 1.1; 2.; 3.; 2.; 1.1; 0.4 |] in
  scal (1. /. 3.) rep;
  test "xcorr x x ~scale:Coeff" (xcorr x x ~scale:Coeff) rep;
  test "xcorr x x ~maxlag:5" (xcorr x x ~maxlag:5)
    (Vec.of_array [| 0.; 0.; 4.; 11.; 20.; 30.; 20.; 11.;  4.; 0.; 0. |]);
  test "xcorr x x ~maxlag:2" (xcorr x x ~maxlag:2)
    (Vec.of_array [| 11.; 20.; 30.; 20.; 11. |]);
  test "xcorr x y" (xcorr x y)
    (Vec.of_array [| 0.; 0.; 2.; 5.; 8.; 11.; 4. |]);
  test "xcorr x y ~maxlag:5" (xcorr x y ~maxlag:5)
    (Vec.of_array [| 0.; 0.; 0.; 0.; 2.; 5.; 8.; 11.; 4.; 0.; 0. |]);
  test "xcorr x y ~maxlag:2" (xcorr x y ~maxlag:2)
    (Vec.of_array [| 0.; 2.; 5.; 8.; 11. |]);
  test "xcorr y x" (xcorr y x)
    (Vec.of_array [| 4.; 11.; 8.; 5.; 2.; 0.; 0. |]);
  let z = Vec.init 25 (fun i -> float i) in
  pp_fvec "z" z;
  let rep = Vec.of_array
    [| 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
       0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.;
       0.; 0.; 0.; 4.; 11.; 20.; 30.; 40.; 50.;
       60.; 70.; 80.; 90.; 100.; 110.; 120.; 130.; 140.;
       150.; 160.; 170.; 180.; 190.; 200.; 210.; 220.; 230.;
       240.; 146.;  74.;  25. |] in
  test "xcorr z x" (xcorr z x) rep;
