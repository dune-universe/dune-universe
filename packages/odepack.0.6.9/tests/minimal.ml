
open Printf
open Bigarray

let fname = Filename.temp_file "odepack" ".dat"

let () =
  let f _ y y' =
    y'.{1} <- y.{2};
    y'.{2} <- -. y.{1} in
  let y = Array1.of_array float64 fortran_layout [| 0.; 1. |] in
  let exact_sol t = (sin t, cos t) in
  let t = 10. in
  let n = 100 in
  let dt = t /. float(n-1) in
  let ode = Odepack.lsoda f y 0. 0. in
  let fh = open_out fname in
  let err = ref 0. in
  for i = 0 to n - 1 do
    let t = float i *. dt in
    let y = Odepack.sol ode t in
    let y1, y2 = exact_sol t in
    let dy1 = y.{1} -. y1 and dy2 = y.{2} -. y2 in
    fprintf fh "%f %f %f %g %g\n" t y.{1} y.{2} dy1 dy2;
    err := max !err (max (abs_float dy1) (abs_float dy2));
  done;
  close_out fh;
  printf "Computed max abs error = %g\n" !err;
  printf "Wrote %S.\n" fname
