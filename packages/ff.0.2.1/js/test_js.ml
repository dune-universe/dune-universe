open Js_of_ocaml

module Fp = Ff.MakeFp (struct
  let prime_order = Z.of_string "53"
end)

let _ =
  Js.export_all
    (object%js
       method add x y = Fp.add x y

       method mul x y = Fp.mul x y

       method zero = Fp.zero

       method one = Fp.one

       method random = Fp.random ()

       method toString x = Fp.to_string x
    end)
