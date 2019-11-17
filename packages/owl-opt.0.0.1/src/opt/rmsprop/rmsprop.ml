module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type fv = AD.t
  type prm = AD.t
  type prms = prm P.t

  type x =
    { p : AD.t
    ; v : AD.t
    }

  type xs = x P.t
  type f = prms -> fv

  type state =
    { xs : xs
    ; f : f
    ; fv : float
    ; k : int
    }

  type stop = state -> bool

  let iter s = s.k
  let prms s = P.map ~f:(fun x -> x.p) s.xs
  let f s = s.f
  let fv s = s.fv

  let init ~prms0 ~f () =
    let fv = AD.unpack_flt (f prms0) in
    let xs =
      P.map
        ~f:(fun p ->
          let v = AD.copy_primal' p in
          AD.Mat.reset v;
          { p; v })
        prms0
    in
    { xs; fv; f; k = 0 }


  let min_update lr x g v = AD.Maths.(x - (lr * g / sqrt v))
  let max_update lr x g v = AD.Maths.(x + (lr * g / sqrt v))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) ?(beta = 0.9) ~lr s =
    let beta = AD.(F beta) in
    let beta_ = AD.(Maths.(F 1. - beta)) in
    let rec run s =
      if stop s
      then s
      else (
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
              let v = AD.Maths.((beta * x.v) + (beta_ * sqr g)) in
              let p =
                match lr with
                | Lr.Fix lr -> update (AD.pack_flt lr) p g v |> AD.primal
                | Lr.Ada h -> update (AD.pack_flt (h s.k)) p g v |> AD.primal
              in
              { p; v })
            xs
        in
        let s = { s with xs; k = succ s.k; fv } in
        run s)
    in
    run s


  let min = optimise min_update
  let max = optimise max_update
end
