module Make (AD : Owl_algodiff_generic_sig.Sig with type A.elt = float) (P : Prms.PT) =
struct
  type prm = AD.t
  type fv = AD.t
  type prms = prm P.t
  type f = prms -> fv

  type state =
    { prms : prms
    ; f : f
    ; fv : float
    ; k : int
    }

  type stop = state -> bool

  let iter s = s.k
  let prms s = s.prms
  let f s = s.f
  let fv s = s.fv

  let init ~prms0 ~f () =
    let fv = AD.unpack_flt (f prms0) in
    { prms = prms0; fv; f; k = 0 }


  let min_update lr x g = AD.Maths.(x - (lr * g))
  let max_update lr x g = AD.Maths.(x + (lr * g))

  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) ~lr s =
    let rec run s =
      if stop s
      then s
      else (
        let t = AD.tag () in
        let prms = P.map ~f:(fun x -> AD.make_reverse x t) s.prms in
        let l = s.f prms in
        AD.(reverse_prop (F 1.) l);
        let fv = AD.unpack_flt l in
        let prms =
          P.map
            ~f:(fun prm ->
              let x = AD.primal prm in
              let g = AD.adjval prm in
              match lr with
              | Lr.Fix lr -> update (AD.pack_flt lr) x g |> AD.primal
              | Lr.Ada h -> update (AD.pack_flt (h s.k)) x g |> AD.primal)
            prms
        in
        let s = { s with prms; k = succ s.k; fv } in
        run s)
    in
    run s


  let min = optimise min_update
  let max = optimise max_update
end
