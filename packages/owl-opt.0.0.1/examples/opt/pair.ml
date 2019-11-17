open Owl
module Prms = Owl_opt.Prms.Pair
module O = Owl_opt.D.Adam.Make (Prms)

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 5 3 in
  let b = Algodiff.D.Mat.gaussian 5 1 in
  let y = Algodiff.D.Maths.((a *@ x) + b) in
  let prms0 = Prms.pack (Algodiff.D.Mat.gaussian 5 3, Algodiff.D.Mat.gaussian 5 1) in
  let f prms =
    let a, b = Prms.unpack prms in
    Algodiff.D.Maths.(l2norm' (y - ((a *@ x) + b)))
  in
  let lr = Owl_opt.Lr.Fix 1E-4 in
  let s0 = O.init ~f ~prms0 () in
  let s = O.min ~lr s0 in
  Printf.printf "\nfinal loss: %f\n" (O.fv s)
