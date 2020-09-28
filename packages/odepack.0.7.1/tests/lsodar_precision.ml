open Printf
open Bigarray

type vec = Odepack.vec
let vec a = Array1.of_array float64 fortran_layout a

let pi = acos(-1.)

let round x = floor(x +. 0.5)  (* naive *)

let f _t (y: vec) (dy: vec) =
  dy.{1} <- y.{2};
  dy.{2} <- -. y.{1}

let g _t (y: vec) (g: vec) =
  g.{1} <- y.{1}

let () =
  let y = vec [| 0.; 1.|] in (* thus, solution is "sin" *)
  let ode = Odepack.lsodar f y 0. 17. ~atol:1e-15 ~rtol:1e-14 ~copy_y0:false
                           ~g ~ng:1 in
  while Odepack.has_root ode do
    let t = Odepack.time ode in
    let k = round(t /. pi) in
    printf "At t = %9.6f = (%g + %.15g)π,  Y = (%g, %g)\n"
      t k (t /. pi -. k) y.{1} y.{2};
    Odepack.advance ode;
  done
