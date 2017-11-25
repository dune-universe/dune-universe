open Printf
open Bigarray
module FFT = Fftw3.D


let () =
  let n0 = 16
  and n1 = 32 in
  printf "Check dft, FORTRAN layout, %i×%i matrix... %!" n0 n1;
  let u = FFT.Array2.create complex64 fortran_layout n0 n1
  and fu = FFT.Array2.create complex64 fortran_layout n0 n1 in
  let forward = FFT.Array2.dft FFT.Forward u fu ~meas:FFT.Measure in
  let backward = FFT.Array2.dft FFT.Backward fu u ~meas:FFT.Measure in
  for i = 1 to n0 do
    for j = 1 to n1 do
      u.{i, j} <- {Complex.re = float(i+j); im = 0.}
    done
  done;
  FFT.exec forward;
  FFT.exec backward;
  (* [u] must be multiplied by d² *)
  let open Complex in
  for i = 1 to n0 do
    for j = 1 to n1 do
      let r = float((i + j) * n0 * n1) in
      if abs_float(u.{i,j}.re -. r) > 1e-10 *. r then
        printf "  u.{%i,%i} = %g (expected %g)\n" i j u.{i,j}.re r
    done
  done;
  printf "DONE\n%!"


let () =
  let n0 = 16
  and n1 = 32 in
  printf "Check r2c & c2r, C layout, %i×%i matrix... %!" n0 n1;
  let u = FFT.Array2.create float64 c_layout n0 n1
  and fu = FFT.Array2.create complex64 c_layout n0 (n1/2 + 1) in
  let forward = FFT.Array2.r2c u fu ~meas:FFT.Measure in
  let backward = FFT.Array2.c2r fu u ~meas:FFT.Measure in
  for i = 0 to n0 - 1 do
    for j = 0 to n1 - 1 do
      u.{i, j} <- float(i+j)
    done
  done;
  FFT.exec forward;
  FFT.exec backward;  (* destroys input *)
  (* [u] must be multiplied by n0 * n1 *)
  for i = 0 to n0 - 1 do
    for j = 0 to n1 - 1 do
      let r = float((i + j) * n0 * n1) in
      if abs_float(u.{i,j} -. r) > 1e-10 *. r then
        printf "  u.{%i,%i} = %g (expected %g)\n" i j u.{i,j} r
    done
  done;
  printf "DONE\n%!"


let () =
  let n0 = 32
  and n1 = 16 in
  printf "Check r2c & c2r, FORTRAN layout, %i×%i matrix... %!" n0 n1;
  let u = FFT.Array2.create float64 fortran_layout n0 n1
  and fu = FFT.Array2.create complex64 fortran_layout (n0/2 + 1) n1 in
  let forward = FFT.Array2.r2c u fu ~meas:FFT.Measure in
  let backward = FFT.Array2.c2r fu u ~meas:FFT.Measure in
  for i = 1 to n0 do
    for j = 1 to n1 do
      u.{i, j} <- float(i+j)
    done
  done;
  FFT.exec forward;
  FFT.exec backward;  (* destroys input *)
  (* [u] must be multiplied by d² *)
  for i = 1 to n0 do
    for j = 1 to n1 do
      let r = float((i + j) * n0 * n1) in
      if abs_float(u.{i,j} -. r) > 1e-10 *. r then
        printf "  u.{%i,%i} = %g (expected %g)\n" i j u.{i,j} r
    done
  done;
  printf "DONE\n%!"


(* Local Variables: *)
(* compile-command: "make -k -C .. tests" *)
(* End: *)
