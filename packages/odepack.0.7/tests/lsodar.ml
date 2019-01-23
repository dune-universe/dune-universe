open Printf
open Bigarray

type vec = Odepack.vec
let vec a = Array1.of_array float64 fortran_layout a

let f _t (y: vec) (dy: vec) =
  dy.{1} <- -0.04 *. y.{1} +. 1e4 *. y.{2} *. y.{3};
  dy.{3} <- 3e7 *. y.{2}**2.;
  dy.{2} <- -. dy.{1} -. dy.{3}

let g _t (y: vec) (g: vec) =
  g.{1} <- y.{1} -. 1e-4;
  g.{2} <- y.{3} -. 1e-2

let () =
  let y = vec [| 1.; 0.; 0. |] in
  let atol_vec = vec [| 1e-6; 1e-10; 1e-6 |] in
  let ode = Odepack.lsodar f y 0. 0. ~atol_vec ~rtol:1e-4 ~copy_y0:false
                           ~g ~ng:2 in
  for i = 0 to 11 do
    Odepack.advance ode ~time:(0.4 *. 10.**(float i));
    printf "At t = %e  Y = %e %e %e\n" (Odepack.time ode) y.{1} y.{2} y.{3};
    while Odepack.has_root ode do
      printf "    The above line is a root:";
      for i = 1 to 2 do printf " %b" (Odepack.root ode i) done;
      printf "\n";
      Odepack.advance ode;
      printf "At t = %e  Y = %e %e %e\n" (Odepack.time ode) y.{1} y.{2} y.{3};
    done
  done

