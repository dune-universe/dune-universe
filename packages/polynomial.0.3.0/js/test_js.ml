open Js_of_ocaml

module Fp = Ff.MakeFp (struct
  let prime_order = Z.of_string "337"
end)

module Poly = Polynomial.MakeUnivariate (Fp)

let _ =
  Js.export_all
    (object%js
       method evaluation_fft generator power polynomial =
         Poly.evaluation_fft ~generator ~power polynomial

       method generate_random_polynomial degree =
         Poly.generate_random_polynomial degree

       method degreeInfinity = Polynomial.Infinity

       method degreeN n = Polynomial.Natural n
    end)
