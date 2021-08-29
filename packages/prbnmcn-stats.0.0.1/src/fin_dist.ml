module Make (Reals : Basic_intf.Reals) = struct
  type reals = Reals.t

  module type Fin_fun = sig
    type t

    module V : Basic_intf.Free_module with type R.t = Reals.t and type basis = t

    val weightmap : V.t
  end

  module type Fin_kernel = sig
    type t

    type u

    module V : Basic_intf.Free_module with type R.t = Reals.t and type basis = u

    val kernel : t -> V.t
  end

  type 'a fin_fun = (module Fin_fun with type t = 'a)

  type 'a fin_den = 'a fin_fun

  (* A finitely supported probability is normalized. *)
  type 'a fin_prb = 'a fin_fun

  type ('a, 'b) kernel = (module Fin_kernel with type t = 'a and type u = 'b)

  let sample_prb : type a. a fin_prb -> Random.State.t -> a =
    fun (type t) (module P : Fin_fun with type t = t) rng_state ->
     let exception Sampled of t in
     let r = Reals.lebesgue rng_state in
     try
       let _ =
         P.V.fold
           (fun elt weight cumu ->
             let cumu = Reals.add cumu weight in
             if Reals.(r <= cumu) then raise (Sampled elt) else cumu)
           P.weightmap
           Reals.zero
       in
       assert false
     with Sampled x -> x

  let density (type t)
      (module V : Basic_intf.Free_module
        with type R.t = Reals.t
         and type basis = t) (elements : (t * Reals.t) list) : t fin_den =
    (module struct
      type nonrec t = t

      module V = V

      let weightmap = V.of_list elements
    end)

  let probability (type t)
      (module V : Basic_intf.Free_module
        with type R.t = Reals.t
         and type basis = t) (elements : (t * Reals.t) list) : t fin_prb =
    let (_points, weights) = List.split elements in
    let total_weight = List.fold_left Reals.add Reals.zero weights in
    if Reals.compare total_weight Reals.one <> 0 then
      invalid_arg "Stats.probability: weights do not sum up to 1" ;
    density (module V) elements

  let total_mass (type t) ((module D) : t fin_den) : Reals.t =
    D.V.fold (fun _ w acc -> Reals.add w acc) D.weightmap Reals.zero

  let normalize (type t) ((module D) : t fin_den) : t fin_prb =
    let mass = total_mass (module D) in
    let imass = Reals.div Reals.one mass in
    (module struct
      type t = D.t

      module V = D.V

      let weightmap = V.smul imass D.weightmap
    end)

  let fin_prb_of_empirical (type t)
      (module V : Basic_intf.Free_module
        with type R.t = Reals.t
         and type basis = t) (p : t array) : t fin_den =
    let weightmap =
      Array.fold_left
        (fun vec elt -> V.add vec (V.of_list [(elt, Reals.one)]))
        V.zero
        p
    in
    let density : t fin_den =
      (module struct
        type nonrec t = t

        module V = V

        let weightmap = weightmap
      end)
    in
    normalize density

  let uniform (type t)
      (module V : Basic_intf.Free_module
        with type R.t = Reals.t
         and type basis = t) (arr : t array) : t fin_prb =
    let len = Array.length arr in
    if Int.equal len 0 then failwith "uniform: empty array"
    else
      let prb = Reals.(div one (of_int len)) in
      (module struct
        type nonrec t = t

        module V = V

        let weightmap =
          Array.fold_left
            (fun map x -> V.add (V.of_list [(x, prb)]) map)
            V.zero
            arr
      end)

  let eval_prb (type t) ((module P) : t fin_prb) (x : t) : Reals.t =
    P.V.eval P.weightmap x

  let integrate (type t) ((module P) : t fin_prb) (f : t -> Reals.t) : Reals.t =
    P.V.fold (fun x w acc -> Reals.(acc + (w * f x))) P.weightmap Reals.zero

  let kernel (type a b) ?(h : (module Basic_intf.Std with type t = a) option)
      (module V : Basic_intf.Free_module
        with type R.t = Reals.t
         and type basis = b) (kernel : a -> (b * Reals.t) list) : (a, b) kernel
      =
    let kernel =
      match h with
      | None -> fun x -> V.of_list (kernel x)
      | Some (module H) -> (
          let module Element = struct
            type t = { key : H.t; data : V.t }

            let hash { key; _ } = H.hash key

            let equal x1 x2 = H.equal x1.key x2.key
          end in
          let module Table = Weak.Make (Element) in
          let table = Table.create 11 in
          fun x ->
            match Table.find_opt table { Element.key = x; data = V.zero } with
            | None ->
                let res = V.of_list (kernel x) in
                Table.add table { Element.key = x; data = res } ;
                res
            | Some { Element.data; _ } -> data)
    in
    let module K = struct
      type t = a

      type u = b

      module V = V

      let kernel = kernel
    end in
    (module K)

  let compose :
      type a b c.
      ?h:(module Basic_intf.Std with type t = a) ->
      (a, b) kernel ->
      (b, c) kernel ->
      (a, c) kernel =
   fun ?h (module K1) (module K2) ->
    let kernel =
      match h with
      | None ->
          fun x ->
            let vec = K1.kernel x in
            K1.V.fold
              (fun b pb acc ->
                let vec = K2.kernel b in
                K2.V.add (K2.V.smul pb vec) acc)
              vec
              K2.V.zero
      | Some (module H) -> (
          let module Element = struct
            type t = { key : H.t; data : K2.V.t }

            let hash { key; _ } = H.hash key

            let equal x1 x2 = H.equal x1.key x2.key
          end in
          let module Table = Weak.Make (Element) in
          let table = Table.create 11 in
          fun x ->
            match
              Table.find_opt table { Element.key = x; data = K2.V.zero }
            with
            | None ->
                let vec = K1.kernel x in
                let res =
                  K1.V.fold
                    (fun b pb acc ->
                      let vec = K2.kernel b in
                      K2.V.(add (smul pb vec) acc))
                    vec
                    K2.V.zero
                in
                Table.add table { Element.key = x; data = res } ;
                res
            | Some { Element.data; _ } -> data)
    in
    let module Kernel = struct
      type t = K1.t

      type u = K2.u

      module V = K2.V

      let kernel = kernel
    end in
    (module Kernel)

  let constant_kernel : type a b. b fin_prb -> (a, b) kernel =
   fun (module Prb) ->
    let module Kernel = struct
      type t = a

      type u = Prb.t

      module V = Prb.V

      let kernel _x = Prb.weightmap
    end in
    (module Kernel)

  let eval_kernel : type a b. a -> (a, b) kernel -> (b * Reals.t) list =
   fun x (module K) -> K.V.fold (fun k p acc -> (k, p) :: acc) (K.kernel x) []

  let raw_data_density (type t) ((module D) : t fin_den) =
    let den = D.V.fold (fun elt w acc -> (elt, w) :: acc) D.weightmap [] in
    `Density den

  let raw_data_probability (type t) ((module D) : t fin_prb) =
    let den = D.V.fold (fun elt w acc -> (elt, w) :: acc) D.weightmap [] in
    `Probability den

  let pp_fin_fun :
      (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a fin_den -> unit
      =
   fun kf f den ->
    let (`Density l) = raw_data_density den in
    Format.fprintf
      f
      "@[<h>%a@]"
      (Format.pp_print_list (fun elt_fmt (elt, pr) ->
           Format.fprintf elt_fmt "(%a, %a);@," kf elt Reals.pp pr))
      l

  let pushforward (type t u) ~(prior : t fin_fun) ~(likelihood : (t, u) kernel)
      : u fin_prb =
    let (module Prior) = prior in
    let (module Likelihood) = likelihood in
    let map =
      Prior.V.fold
        (fun x px acc ->
          let fx = Likelihood.kernel x in
          Likelihood.V.(add (smul px fx) acc))
        Prior.weightmap
        Likelihood.V.zero
    in
    let module Result = struct
      type t = u

      module V = Likelihood.V

      let weightmap = map
    end in
    (module Result)

  let inverse (type t u) ?(h : (module Basic_intf.Std with type t = u) option)
      (prior : t fin_prb) (likelihood : (t, u) kernel) :
      u fin_prb * (u, t) kernel =
    let (module Prior) = prior in
    let (module Likelihood) = likelihood in
    let (module Pushforward) = pushforward ~prior ~likelihood in
    let kernel (y : u) =
      let nu_y = Pushforward.V.eval Pushforward.weightmap y in
      Prior.V.fold
        (fun x mu_x acc ->
          let forward = Likelihood.kernel x in
          let f_x_y = Likelihood.V.eval forward y in
          let prob = Reals.(mul mu_x f_x_y / nu_y) in
          Prior.V.(add acc (of_list [(x, prob)])))
        Prior.weightmap
        Prior.V.zero
    in
    let module Kernel = struct
      type t = Likelihood.u

      type u = Likelihood.t

      module V = Prior.V

      let kernel =
        match h with
        | None -> kernel
        | Some (module H) -> (
            let module Element = struct
              type t = { key : H.t; data : V.t }

              let hash { key; _ } = H.hash key

              let equal x1 x2 = H.equal x1.key x2.key
            end in
            let module Table = Weak.Make (Element) in
            let table = Table.create 11 in
            fun y ->
              match
                Table.find_opt table { Element.key = y; data = Prior.V.zero }
              with
              | None ->
                  let res = kernel y in
                  Table.add table { Element.key = y; data = res } ;
                  res
              | Some res -> res.data)
    end in
    ((module Pushforward), (module Kernel))

  module Bool_vec =
    Basic_impl.Free_module.Make (Std.Bool) (Reals) (Basic_impl.Bool_map)
  module Int_vec =
    Basic_impl.Free_module.Make (Std.Int) (Reals) (Basic_impl.Int_map)

  let coin ~bias : bool fin_prb =
    if Reals.(bias < zero || bias > one) then
      failwith "Stats.coin: invalid bias"
    else density (module Bool_vec) [(true, bias); (false, Reals.(one - bias))]

  let bincoeff n k =
    let n = Reals.of_int n in
    let rec loop i acc =
      if Int.equal i (k + 1) then acc
      else
        let fi = Reals.of_int i in
        loop (i + 1) Reals.(acc * ((n + one - fi) / fi))
    in
    loop 1 Reals.one

  let binomial (coin : bool fin_prb) n =
    let p = eval_prb coin true in
    let not_p = eval_prb coin false in
    let elements =
      List.init n (fun k ->
          let n_minus_k = n - k in
          Reals.(k, bincoeff n k * npow p k * npow not_p n_minus_k))
    in
    density (module Int_vec) elements

  let mean_generic (type elt)
      (module L : Basic_intf.Module with type t = elt and type R.t = Reals.t)
      ((module Dist) : elt fin_fun) =
    Dist.V.fold (fun x w acc -> L.add (L.smul w x) acc) Dist.weightmap L.zero

  let mean ((module Dist) : reals fin_fun) =
    integrate (module Dist) (fun x -> x)

  let variance ((module Dist) : reals fin_fun) =
    let m = mean (module Dist) in
    Dist.V.fold
      (fun x w acc ->
        let open Reals in
        let delta = x - m in
        let delta_squared = delta * delta in
        acc + (delta_squared * w))
      Dist.weightmap
      Reals.zero
end
