open Owl

module P = struct
  type 'a t =
    { a : 'a
    ; b : 'a
    }
  [@@deriving prms]
end

open P
module O = Owl_opt_lbfgs.Make (P)

let () =
  let x = Algodiff.D.Mat.gaussian 3 1 in
  let a = Algodiff.D.Mat.gaussian 10 3 in
  let b = Algodiff.D.Mat.gaussian 10 1 in
  let y = Algodiff.D.Maths.((a *@ x) + b) in
  let prms0 = { a = Algodiff.D.Mat.gaussian 10 3; b = Algodiff.D.Mat.gaussian 10 1 } in
  let f prms = Algodiff.D.Maths.(l2norm' (y - ((prms.a *@ x) + prms.b))) in
  let stop s =
    let fv = O.fv s in
    Printf.printf "iter %i: %4.10f\n%!" O.(iter s) fv;
    O.(fv s) < 1E-5
  in
  let s0 = O.init ~f ~prms0 () in
  let s = O.min ~stop s0 in
  let fv = (O.f s) O.(prms s) |> Algodiff.D.unpack_flt in
  Printf.printf "iter %i: %1.10f\n%!" O.(iter s) fv
