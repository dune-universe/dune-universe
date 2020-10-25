(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module OpFloat = Fadbad.OpFloat
module T = Fadbad.T(OpFloat)

let f x y =
  let open T in
  let z = sqrt x in
  y * z + (sin z)

let _ =
  let x = T.make 1. in (* Initialize variable x *)
  let y = T.make 2. in (* Initialize variable y *)

  T.set x 1 (OpFloat.one ()); (* Taylor-expand wrt. x (dx/dx=1) *)

  let res = f x y in (* Evaluate function and record DAG *)

  ignore (T.eval res 10); (* Taylor-expand f to degree 10 *)
  (* T.d res i now contains the i-th Taylor-coefficient *)

  Printf.printf "f(x,y)=%f\n" (T.get res); (* Value of function *)

  for i = 0 to 10 do
    let c = T.d res i in (* The i-th Taylor-coefficient *)
    Printf.printf "(1/k!)*(d^%df/dx^%d)=%f\n" i i c
  done;

  T.reset res; (* Reset the values in the DAG *)

  T.set x 0 (OpFloat.make 3.); (* New value for x *)
  T.set y 0 (OpFloat.make 4.); (* New value for y *)
  T.set y 1 (OpFloat.one ()); (* Taylor-expand wrt. y (dy/dy=1) *)
  ignore(T.eval res 10); (* Taylor-expand f to degree 10 *)

  for i = 0 to 10 do
    let c = T.d res i in (* The i-th Taylor-coefficient *)
    Printf.printf "(1/k!)*(d^%df/dy^%d)=%f\n" i i c
  done
