open Bigarray
open Complex
open Printf

module FFT = Fftw3.D

let fft_r2c data =
  let dim = Array.length data in
  let x = FFT.Array1.create FFT.complex c_layout dim in
  let y = FFT.Array1.create FFT.complex c_layout dim in (* Fourier vars *)
  let plan = FFT.Array1.dft FFT.Forward x y in
  Array.iteri (fun i v -> x.{i} <- { re = v ; im = 0.0 }) data ;
  FFT.exec plan;
  Array.init dim (fun i -> y.{i})

let _ =
  let data = Array.make 10 1.0 in
  let y = fft_r2c data in
  Array.iter (fun x -> printf "% f%+fi\n" x.re x.im) y
