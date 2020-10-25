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

let s = 10.
let r = 28.
let q = float (8 / 3)

type lorenz = {
  x : T.t; y : T.t; z : T.t; p : T.t; (* independent variables *)
  xp : T.t; yp : T.t; zp : T.t; pp : T.t; (* dependent variables *)
}

let create_lorenz () =
  let open T in
  let x = create () in
  let y = create () in
  let z = create () in
  let p = create () in
  let rmz = translate (-z) r in
  let xp = p * (scale (y-x) s) in
  let yp = p * (x * rmz - y) in
  let zp = p * ((x * y) - (scale z q)) in
  let pp = make 0. in
  { x; y; z; p; xp; yp; zp; pp }

let reset_lorenz l =
  let open T in
  reset l.xp;
  reset l.yp;
  reset l.zp;
  reset l.pp

let _ =
  (* construct ODE *)
  let ode = create_lorenz () in

  (* set point of expansion *)
  T.set ode.x 0 (OpFloat.make 2.14736765);
  T.set ode.y 0 (OpFloat.make (-2.07804819));
  T.set ode.z 0 (OpFloat.make (r-.1.));
  T.set ode.p 0 (OpFloat.make 1.55865218);

  for i = 0 to 9 do
    (* evaluate the i-th Taylor coefficient of the r.h.s of the ODE *)
    ignore(T.eval ode.xp i);
    ignore(T.eval ode.yp i);
    ignore(T.eval ode.zp i);
    ignore(T.eval ode.pp i);

    (* since d(x,y,z,p)/dt = lorenz(x,y,z,p) we have *)
    T.set ode.x (i+1) OpFloat.((T.deriv ode.xp i) / (integer Stdlib.(i+1)));
    T.set ode.y (i+1) OpFloat.((T.deriv ode.yp i) / (integer Stdlib.(i+1)));
    T.set ode.z (i+1) OpFloat.((T.deriv ode.zp i) / (integer Stdlib.(i+1)));
    T.set ode.p (i+1) OpFloat.((T.deriv ode.pp i) / (integer Stdlib.(i+1)));
  done;

  (* print out the Taylor coefficients for the solution of the ODE *)
  for i = 0 to 10 do
    Printf.printf "x[%d]=%f\n" i (T.d ode.x i);
    Printf.printf "y[%d]=%f\n" i (T.d ode.y i);
    Printf.printf "z[%d]=%f\n" i (T.d ode.z i);
    Printf.printf "p[%d]=%f\n" i (T.d ode.p i);
  done
