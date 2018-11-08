
open Printf
open Bigarray

type vec = Odepack.vec
let vec a = Array1.of_array float64 fortran_layout a

let f _t (y: vec) (y': vec) =
  y'.{1} <- -0.04 *. y.{1} +. 1e4 *. y.{2} *. y.{3};
  y'.{3} <- 3e7 *. y.{2} *. y.{2};
  y'.{2} <- -. y'.{1} -. y'.{3}

let () =
  let y = vec [| 1.; 0.; 0. |] in
  let atol_vec = vec [| 1e-6; 1e-10; 1e-6 |] in
  let ode = Odepack.lsoda f y 0. 0. ~atol_vec ~rtol:1e-4 ~copy_y0:false in
  for i = 0 to 11 do
    let t = 0.4 *. 10.**(float i) in
    Odepack.advance ode ~time:t;
    printf "At t = %e  Y = %e %e %e\n" t y.{1} y.{2} y.{3};
  done
