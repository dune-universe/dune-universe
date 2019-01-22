(* Moré tests taken from:
  Moré, J.J., Garbow, B.S. and Hillstrom, K.E., Testing Unconstrained
  Optimization Software, ACM Trans. Math. Software 7 (1981), 17-41.

  See also: http://www.netlib.org/uncon/data/
  http://www.mat.univie.ac.at/~neum/glopt/bounds.html
  http://www.uni-graz.at/imawww/kuntsevich/solvopt/results/moreset.html *)

open Format
open Bigarray
open Lacaml.D

type vec = Lbfgs.F.vec
let vec a = Array1.of_array float64 fortran_layout a

let pi = 4. *. atan 1.

(* Min g : x ↦ ∑ f²_i(x).  [f x v] does [v ← f(x)] and [jacf x jac]
  does [jac.{i,j} ← ∂_j f_i(x)], i=1,...,m, j=1,...,n. *)
let least_square name f jacf ?m ?sol x0 =
  let n = Array1.dim x0 in
  let m = match m with Some m -> m | None -> n in
  let v = Array1.create float64 fortran_layout m
  and jac = Array2.create float64 fortran_layout m n in
  let g_dg (x: vec) (grad: vec) =
    f x v;
    jacf x jac;
    ignore(gemv jac ~trans:`T v ~y:grad); (* y ← m^T v *)
    Mat.scal 2. jac;
    Vec.sqr_nrm2 v ~stable:true
  in
  let g_min = Lbfgs.F.min g_dg x0 ~factr:1e1 ~pgtol:1e-12 in
  printf "@[<h>%-19s: min=%g at@ (" name g_min;
  Lacaml.Io.pp_rfvec std_formatter x0;
  match sol with
  | None -> printf ")@]@\n"
  | Some x ->
    let g_sol = g_dg x (Array1.create float64 fortran_layout n) in
    printf "),@ sol=%g at (" g_sol;
    Lacaml.Io.pp_rfvec std_formatter x;
    printf ")@]@\n%!"

let () =
  let f x v =
    v.{1} <- 10. *. (x.{2} -. x.{1} *. x.{1});
    v.{2} <- 1. -. x.{1}
  and jacf x m =
    m.{1,1} <- -20. *. x.{1};
    m.{1,2} <- 10.;
    m.{2,1} <- -1.;
    m.{2,2} <- 0. in
  least_square "Rosenbrock" f jacf (vec [|-1.2; 1.|]) ~sol:(vec [|1.; 1.|])

let () =
  let f x v =
    v.{1} <- -13. +. x.{1} +. ((5. -. x.{2}) *. x.{2} -. 2.) *. x.{2};
    v.{2} <- -29. +. x.{1} +. ((x.{2} +. 1.) *. x.{2} -. 14.) *. x.{2}
  and jacf x m =
    m.{1,1} <- 1.;   m.{1,2} <- (10. -. 3. *. x.{2}) *. x.{2} -. 2.;
    m.{2,1} <- 1.;   m.{2,2} <- (3. *. x.{2} +. 2.) *. x.{2} -. 14.  in
  least_square "Freudenstein & Roth" f jacf (vec [|0.5; -2.|])
               ~sol:(vec [|11.41; -0.8968|])
(* The paper also gives 0. at (5.; 4), the absolute min. *)

let () =
  let f x v =
    v.{1} <- 1e4 *. x.{1} *. x.{2} -. 1.;
    (* v.{2} <- exp(-. x.{1}) +. exp(-. x.{2}) -. 1.0001; *)
    if abs_float x.{1} <= abs_float x.{2} then
      v.{2} <- (expm1(-. x.{1}) -. 0.0001) +. exp(-. x.{2})
    else
      v.{2} <- (expm1(-. x.{2}) -. 0.0001) +. exp(-. x.{1});
  and jacf x m =
    m.{1,1} <- 1e4 *. x.{2};        m.{1,2} <- 1e4 *. x.{1};
    m.{2,1} <- -. exp(-. x.{1});   m.{2,2} <- -. exp(-. x.{2}) in
  least_square "Powell badly scaled" f jacf (vec [|0.; 1.|])
               ~sol:(vec [|1.098e-5; 9.106|])
(* The min is not found correctly.  Same results as reported in:
   http://www.mini.pw.edu.pl/~mkobos/programs/lbfgsb_wrapper/index.html *)

let () =
  let f x v =
    v.{1} <- x.{1} -. 1e6;
    v.{2} <- x.{2} -. 2e-6;
    v.{3} <- x.{1} *. x.{2} -. 2.;
  and jacf x m =
    m.{1,1} <- 1.;     m.{1,2} <- 0.;
    m.{2,1} <- 0.;     m.{2,2} <- 1.;
    m.{3,1} <- x.{2};  m.{3,2} <- x.{1} in
  least_square "Brown badly scaled" f jacf ~m:3 (vec [|1.; 1.|])
               ~sol:(vec [|1e6; 2e-6|])

let () =
  let f x v =
    v.{1} <- 1.5   -. x.{1} *. (1. -. x.{2});
    v.{2} <- 2.25  -. x.{1} *. (1. -. x.{2} *. x.{2});
    v.{3} <- 2.625 -. x.{1} *. (1. -. x.{2} *. x.{2} *. x.{2});
  and jacf x m =
    m.{1,1} <- x.{2} -. 1.;           m.{1,2} <- x.{1};
    m.{2,1} <- x.{2} *. x.{2} -. 1.;   m.{2,2} <- x.{1} *. 2. *. x.{2};
    m.{3,1} <- x.{2} *. x.{2} *. x.{2} -. 1.;
    m.{3,2} <- x.{1} *. 3. *. x.{2} *. x.{2} in
  least_square "Beale" f jacf ~m:3 (vec [|1.; 1.|]) ~sol:(vec [|3.; 0.5|])

let () =
  let m = 10 in (* The value for which the sol is given *)
  assert(m >= 2 (* = n *));
  let f x v =
    for i = 1 to m do
      let i' = float i in
      v.{i} <- 2. +. 2. *. i' -. (exp(i' *. x.{1}) +. exp(i' *. x.{2}));
    done
  and jacf x jac =
    for i = 1 to m do
      let i' = float i in
      jac.{i,1} <- -. i' *. exp(i' *. x.{1});
      jac.{i,2} <- -. i' *. exp(i' *. x.{2});
    done in
  least_square "Jennrich & Sampson" f jacf ~m (vec [|0.3; 0.4|])
               ~sol:(vec [|0.2578; 0.2578|]) (* at level 124.362... ! *)

let () =
  let theta x1 x2 =
    if x1 >= 0. then 1. /. (2. *. pi) *. atan(x2 /. x1)
    else 1. /. (2. *. pi) *. atan(x2 /. x1) +. 0.5 in
  let f x v =
    v.{1} <- 10. *. (x.{3} -. 10. *. theta x.{1} x.{2});
    v.{2} <- 10. *. (sqrt(x.{1} *. x.{1} +. x.{2} *. x.{2}) -. 1.);
    v.{3} <- x.{3} in
  let jacf x jac =
    let d2 = x.{1} *. x.{1} +. x.{2} *. x.{2} in
    jac.{1,1} <-  50. /. pi *. x.{2} /. d2;
    jac.{1,2} <- -50. /. pi *. x.{1} /. d2;
    jac.{1,3} <- 10.;
    let d = 10. /. sqrt d2 in
    jac.{2,1} <- d *. x.{1} ;   jac.{2,2} <- d *. x.{2};   jac.{2,3} <- 0.;
    jac.{3,1} <- 0.;           jac.{3,2} <- 0.;          jac.{3,3} <- 1. in
  least_square "Helical valley" f jacf (vec [|-1.; 0.; 0.|])
               ~sol:(vec [|1.; 0.; 0.|])

let () =
  let m = 15 in
  let y = vec [| 0.14; 0.18; 0.22; 0.25; 0.29;
                0.32; 0.35; 0.39; 0.37; 0.58;
                0.73; 0.96; 1.34; 2.10; 4.39 |] in
  let u = Vec.init m float
  and v = Vec.init m (fun i -> float(16 - i)) in
  let w = Vec.init m (fun i -> min u.{i} v.{i}) in
  let f x v =
    for i = 1 to m do
      v.{i} <- y.{i} -. (x.{1} +. u.{i} /. (v.{i} *. x.{2} +. w.{i} *. x.{3}))
    done in
  let jacf x jac =
    for i = 1 to m do
      jac.{i,1} <- -1.;
      let d = (v.{i} *. x.{2} +. w.{i} *. x.{3})**2. in
      jac.{i,2} <- u.{i} *. v.{i} /. d;
      jac.{i,3} <- u.{i} *. w.{i} /. d;
    done in
  (* Roundoff error seem to dominate the computation of the function
     to minimize. *)
  least_square "Bard" f jacf ~m (vec [|1.; 1.; 1.|])
               (* 17.4286... at (0.8406...; -∞; -∞);
                  0.00821487 at (0.0824106; 1.13304; 2.3437) *)

let () =
  let m = 15 in
  let y = vec [|0.0009; 0.0044; 0.0175; 0.0540; 0.1295; 0.2420; 0.3521; 0.3989;
               0.3521; 0.2420; 0.1295; 0.0540; 0.0175; 0.0044; 0.0009 |] in
  let t = Vec.init m (fun i -> 0.5 *. float(8 - i)) in
  let f x v =
    for i = 1 to m do
      v.{i} <- x.{1} *. exp(-0.5 *. x.{2} *. (t.{i} -. x.{3})**2.) -. y.{i}
    done in
  let jacf x jac =
    for i = 1 to m do
      let e = exp(-0.5 *. x.{2} *. (t.{i} -. x.{3})**2.) in
      jac.{i,1} <- e;
      jac.{i,2} <- x.{1} *. e *. (-0.5 *. (t.{i} -. x.{3})**2.);
      jac.{i,3} <- x.{1} *. e *. x.{2} *. (t.{i} -. x.{3});
    done in
  least_square "Gaussian" f jacf ~m (vec [|0.4; 1.; 0.|])
               ~sol:(vec [|0.398956; 1.; 0.|])
               (* min: 1.12793 10⁸ according to Moré & al. *)

let () =
  let m = 16 in
  let y = vec [|34780.; 28610.; 23650.; 19630.; 16370.; 13720.; 11540.; 9744.;
               8261.; 7030.; 6005.; 5147.; 4427.; 3820.; 3307.; 2872. |] in
  let t = Vec.init m (fun i -> float(45 + 5 * i)) in
  let f x v =
    for i = 1 to m do
      v.{i} <- x.{1} *. exp(x.{2} /. (t.{i} +. x.{3})) -. y.{i}
    done in
  let jacf x jac =
    for i = 1 to m do
      let e = exp(x.{2} /. (t.{i} +. x.{3})) in
      let d = t.{i} +. x.{3} in
      jac.{i,1} <- e;
      jac.{i,2} <- x.{1} *. e /. d;
      jac.{i,3} <- x.{1} *. e *. (-. x.{2} /. (d *. d));
    done in
  least_square "Meyer" f jacf ~m (vec [|0.02; 4000.; 250.|])
               (* min: 87.9458 *)


let () =
  let m = 6 in (* other values of m have problems *)
  assert((* n = *) 3 <= m && m <= 100);
  let t = Vec.init m (fun i -> float i /. 100.) in
  let y = Vec.init m (fun i -> 25. +. (-50. *. log t.{i})**(2. /. 3.)) in
  let f x v =
    assert(x.{1} <> 0.);
    for i = 1 to m do
      v.{i} <- exp(-. ((abs_float(x.{2} -. y.{i}))**x.{3}) /. x.{1}) -. t.{i}
    done in
  let jacf x jac =
    for i = 1 to m do
      let x_y = x.{2} -. y.{i} in
      let a = abs_float x_y in
      let b = -. (a**x.{3}) /. x.{1} in
      let e = exp b in
      jac.{i,1} <- e *. (-. b /. x.{1});
      jac.{i,2} <- e *. x.{3} *. b /. x_y;
      jac.{i,3} <- e *. b *. log a
    done in
  least_square "Gulf R&D" f jacf ~m (vec [|5.; 2.5; 0.15|])
               ~sol:(vec [|50.; 25.; 1.5|]) (* => min = 0 *)



(* Local Variables: *)
(* compile-command: "make -k -C .." *)
(* End: *)
