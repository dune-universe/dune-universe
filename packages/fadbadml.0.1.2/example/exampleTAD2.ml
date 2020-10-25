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

type tode = {
  x : T.t; (* independent variable *)
  xp : T.t; (* dependent variable *)
}

let create_tode () =
  let x = T.create () in
  let xp = T.cos x in (* record DAG at construction *)
  { x; xp }

let _ =
  let ode = create_tode () in (* construct ode  *)
  let () = T.set ode.x 0 (OpFloat.make 1.) in (* set point of expansion *)

  for i = 0 to 9 do
    ignore(T.eval ode.xp i); (* evaluate the i-th Taylor coefficient *)

    (* use dx/dt=ode(x)*)
    T.set ode.x (i+1) OpFloat.((T.deriv ode.xp i) / (integer Stdlib.(i+1)))
  done;

  (* T.d ode.x i now contains the i-th Taylor coefficient of
     the solution of the ODE *)

  (* Print out the Taylor coefficients for the solution of the ODE *)

  for i = 0 to 10 do
    Printf.printf "x[%d]=%f\n" i (T.d ode.x i);
  done
