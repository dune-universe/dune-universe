(* Shows an example of an error being raised by LBFGS. *)

open Printf
open Bigarray

let () =
  let f x = x *. x -. 2.
  and f' x = -. x in (* wrong derivative *)
  let u = Array1.create float64 fortran_layout 1 in
  u.{1} <- 1.;
  try
    let m = Lbfgs.F.min (fun u df -> df.{1} <- f' u.{1}; f u.{1}) u
      ~print:(Lbfgs.Every 1) in
    printf "min = %g at x = %g\n" m u.{1};
    exit 1
  with Lbfgs.Abnormal(fx, err) ->
    printf "ERROR: %S at x = %g, f = %g\n" err u.{1} fx;
    printf "(The above error is expected.)\n"
