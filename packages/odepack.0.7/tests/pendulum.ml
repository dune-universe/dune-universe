open Bigarray
open Printf
module ODE = Odepack

let vec v = Array1.of_array float64 fortran_layout v

let l = 2.   (* m *)
let m1 = 2.  (* kg *)
and m2 = 1.  (* kg *)
let k = 5.   (* kg/m², spring rigidity constant *)
let g = 9.81 (* m/s² *)
let a = l /. 2.

let gl = -. g /. l
let al = a *. a *. k /. (l *. l)

let vec_field _t (u:ODE.vec) (du:ODE.vec) =
  (* eprintf "%g %g %g\n%!" _t u.{1} u.{3}; *)
  let dth = u.{3} -. u.{1} in (* θ₂ - θ₁ *)
  du.{1} <- u.{2}; (* ∂θ₁ <- θ₁' *)
  du.{2} <- gl *. sin u.{1} +. al /. m1 *. dth; (* ∂²θ₁ *)
  du.{3} <- u.{4}; (* ∂θ₂ <- θ₂' *)
  du.{4} <- gl *. sin u.{3} -. al /. m2 *. dth (* ∂²θ₂ *)

let () =
  let cond_init = vec [| 1.; 0.; 0.; 0. |] in
  (* http://caml.inria.fr/mantis/view.php?id=5175 *)
  let ode = ODE.lsoda vec_field cond_init 0. 10. in
  let t2 = (ODE.vec ode).{3} in
  for i = 1 to 100_000 do
    ignore(ODE.advance ode ~time:(float(10 + i)))
  done;
  let good = abs_float(t2 -. (-0.77743151685)) < 1e-6 in
  printf "%g (%s)\n" t2 (if good then "good" else "bad")
