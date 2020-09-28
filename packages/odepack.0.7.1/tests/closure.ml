(* Use lsoda on a closure with a variable bound. *)

open Printf
open Bigarray
module ODE = Odepack

let vec_field a _t (u:ODE.vec) (du:ODE.vec) =
  du.{1} <- a *. u.{2};
  du.{2} <- u.{1}

let () =
  let u0 = Array1.of_array float64 fortran_layout [| 1.; 2. |] in
  for i = 10 to 100 do
    let ode = ODE.lsoda (vec_field 1.) u0 0. (float i) in
    printf "%2i: u.{1} = %g\n%!" i (ODE.vec ode).{1}
  done
