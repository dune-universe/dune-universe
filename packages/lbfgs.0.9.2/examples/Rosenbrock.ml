
open Printf
open Bigarray
type vec = Lbfgs.F.vec

let f_df (x: vec) (g: vec) =
  let n = Array1.dim x in
  (* Compute the gradient *)
  let t1 = ref(x.{2} -. x.{1}**2.) in
  g.{1} <- 2. *. (x.{1} -. 1.) -. 16. *. x.{1} *. !t1;
  for i = 2 to n - 1 do
    let t2 = !t1 in
    t1 := x.{i+1} -. x.{i}**2.;
    g.{i} <- 8. *. t2 -. 16. *. x.{i} *. !t1;
  done;
  g.{n} <- 8. *. !t1;
  (* Compute the function value and return it *)
  let f = ref(let d1 = x.{1} -. 1. in 0.25 *. d1 *. d1) in
  for i = 2 to n do
    let di = x.{i} -. x.{i-1} *. x.{i-1} in
    f := !f +. di *. di;
  done;
  4. *. !f

let () =
  let n = 100000 in
  printf "min Rosenbrock function (n = %i): %!" n;
  let x = Array1.create float64 fortran_layout n in
  Array1.fill x 3.;
  let l = Array1.create float64 fortran_layout n in
  for i = 1 to n do
    l.{i} <- if i mod 2 = 1 then 1. else -100.
  done;
  let u = Array1.create float64 fortran_layout n in
  Array1.fill u 100.;

  let f = Lbfgs.F.min f_df x ~l ~u ~corrections:5 ~print:(Lbfgs.Every 1) in
  printf "min = %g\n" f
