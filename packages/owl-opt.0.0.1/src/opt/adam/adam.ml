module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t
  type f = prms -> fv

  type x =
    { p : AD.t
    ; m : AD.t
    ; v : AD.t
    }

  type xs = x P.t

  type state =
    { xs : xs
    ; f : f
    ; fv : float
    ; k : int
    }

  type stop = state -> bool

  let iter s = s.k
  let prms s = P.map s.xs ~f:(fun x -> x.p)
  let f s = s.f
  let fv s = s.fv

  let init ~prms0 ~f () =
    let fv = AD.unpack_flt (f prms0) in
    let xs =
      P.map prms0 ~f:(fun p ->
          let m = AD.copy_primal' p in
          AD.Mat.reset m;
          let v = AD.copy_primal' p in
          AD.Mat.reset v;
          { p; m; v })
    in
    { xs; fv; f; k = 0 }


  let min_update lr x m v eps = AD.Maths.(x - (lr * m / (sqrt v + eps)))
  let max_update lr x m v eps = AD.Maths.(x + (lr * m / (sqrt v + eps)))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) ?(beta1 = 0.9) ?(beta2 = 0.999) ?(eps = 1E-8) ~lr s =
    let beta1 = AD.(F beta1) in
    let beta1_ = AD.(Maths.(F 1. - beta1)) in
    let beta2 = AD.(F beta2) in
    let beta2_ = AD.(Maths.(F 1. - beta2)) in
    let eps = AD.(F eps) in
    let rec run s b1 b2 =
      if stop s
      then s
      else (
        let b1 = AD.Maths.(b1 * beta1) in
        let b2 = AD.Maths.(b2 * beta2) in
        let t = AD.tag () in
        let xs =
          P.map
            ~f:(fun x ->
              let p = AD.make_reverse x.p t in
              { x with p })
            s.xs
        in
        let l = s.f (P.map ~f:(fun x -> x.p) xs) in
        AD.(reverse_prop (F 1.) l);
        let fv = AD.unpack_flt l in
        let xs =
          P.map
            ~f:(fun x ->
              let p = AD.primal x.p in
              let g = AD.adjval x.p in
              (* first moment *)
              let m = AD.Maths.((beta1 * x.m) + (beta1_ * g)) in
              (* bias-corrected first moment *)
              let m_ = AD.Maths.(m / (F 1. - b1)) in
              (* second moment *)
              let v = AD.Maths.((beta2 * x.v) + (beta2_ * sqr g)) in
              (* bias-corrected second moment *)
              let v_ = AD.(Maths.(v / (F 1. - b2))) in
              let p =
                match lr with
                | Lr.Fix lr -> update (AD.pack_flt lr) p m_ v_ eps |> AD.primal
                | Lr.Ada h -> update (AD.pack_flt (h s.k)) p m_ v_ eps |> AD.primal
              in
              { p; m; v })
            xs
        in
        let s = { s with xs; k = succ s.k; fv } in
        run s b1 b2)
    in
    run s AD.(F 1.) AD.(F 1.)


  let min = optimise min_update
  let max = optimise max_update
end
