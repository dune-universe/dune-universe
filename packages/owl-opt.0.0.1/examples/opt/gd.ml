open Owl

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

module O = Owl_opt.D.Gd.Make (P)
open P

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 5 3 in
  let b = Algodiff.D.Mat.gaussian 5 1 in
  let y = Algodiff.D.Maths.((a *@ x) + b) in
  let prms0 = { a = Algodiff.D.Mat.gaussian 5 3; b = Algodiff.D.Mat.gaussian 5 1 } in
  let f prms = Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  let lr = Owl_opt.Lr.Fix 1E-4 in
  let s0 = O.init ~f ~prms0 () in
  let stop s =
    let k = O.iter s in
    let fv = O.fv s in
    Printf.printf "\riter: %i | loss: %4.6f" k fv;
    fv < 1E-3
  in
  let s = O.min ~lr ~stop s0 in
  Printf.printf "\nfinal loss: %f\n%!" (O.fv s)
