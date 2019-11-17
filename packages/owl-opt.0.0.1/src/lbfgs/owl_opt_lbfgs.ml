open Owl
open Bigarray

module Make (P : Owl_opt.Prms.PT) = struct
  type fv = Algodiff.D.t
  type prm = Algodiff.D.t
  type prms = prm P.t
  type f = prms -> fv

  type adt =
    | S
    | M

  type info = (int * int * int array * adt) P.t

  let build_info : prms -> int * info =
   fun prms ->
    let n_prms = ref 0 in
    let info =
      P.map
        ~f:(fun x ->
          let open Algodiff.D in
          match x with
          | F _a ->
            let idx = !n_prms in
            n_prms := !n_prms + 1;
            idx, 1, [| 1 |], S
          | Arr _a ->
            let idx = !n_prms in
            let n = numel x in
            n_prms := !n_prms + n;
            idx, n, shape x, M
          | _ -> assert false)
        prms
    in
    !n_prms, info


  let extract info x =
    P.map
      ~f:(fun (idx, l, s, adt) ->
        match adt with
        | S -> Algodiff.D.pack_flt x.{idx}
        | M ->
          Bigarray.Array1.sub x idx l
          |> Bigarray.genarray_of_array1
          |> (fun x -> Bigarray.reshape x s)
          |> Algodiff.D.pack_arr)
      info


  (* blit a: Algodiff.t into b: Array1.t *)
  let blit f info src dst =
    let open Bigarray in
    let dst =
      P.map
        ~f:(fun (idx, l, s, _) ->
          Array1.sub dst idx l |> genarray_of_array1 |> fun x -> reshape x s)
        info
    in
    let open Algodiff.D in
    P.iter2 src dst ~f:(fun a b ->
        match f a with
        | F x -> Genarray.set b [| 0 |] x
        | Arr x -> Genarray.blit x b
        | _ -> assert false)


  type state =
    { prms : prms
    ; n_prms : int
    ; f : f
    ; fv : float
    ; k : int
    ; info : info
    }

  type stop = state -> bool

  let iter s = s.k
  let fv s = s.fv
  let prms s = s.prms
  let f s = s.f

  let init ~prms0 ~f () =
    let fv = f prms0 |> Algodiff.D.unpack_flt in
    let n_prms, info = build_info prms0 in
    { prms = prms0; n_prms; fv; info; f; k = 0 }


  let f_df s x g =
    let x = extract s.info x in
    let t = Algodiff.D.tag () in
    let x = P.map x ~f:(fun x -> Algodiff.D.make_reverse x t) in
    let c = s.f x in
    Algodiff.D.reverse_prop (F 1.) c;
    blit Algodiff.D.adjval s.info x g;
    Algodiff.D.unpack_flt c


  let stop s =
    if s.k mod 10 = 0 then Printf.printf "\rstep: %i | loss: %4.9f%!" s.k s.fv;
    s.fv < 1E-3


  let optimise update ?(stop = stop) ?(pgtol = 1E-5) ?(factr = 1E7) ?(corrections = 10) s
    =
    let k = ref 0 in
    let ps = reshape_1 Owl.Arr.(zeros [| 1; s.n_prms |]) s.n_prms in
    let stop st =
      let fv = Lbfgs.previous_f st in
      let prms = extract s.info ps in
      k := Lbfgs.iter st;
      stop { s with fv; k = !k; prms }
    in
    blit Algodiff.D.primal s.info s.prms ps;
    let fv = update ~pgtol ~factr ~corrections ~stop (f_df s) ps in
    let prms = extract s.info ps in
    { s with fv; prms; k = !k + 1 }


  let lmin ~pgtol ~factr ~corrections ~stop f_df ps =
    Lbfgs.C.min ~print:Lbfgs.No ~pgtol ~factr ~corrections ~stop f_df ps


  let lmax ~pgtol ~factr ~corrections ~stop f_df ps =
    Lbfgs.C.max ~print:Lbfgs.No ~pgtol ~factr ~corrections ~stop f_df ps


  let min = optimise lmin
  let max = optimise lmax
end
