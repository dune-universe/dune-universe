let measure (type t r) (module R : Basic_intf.Reals with type t = r)
    (module V : Basic_intf.Free_module_std
      with type R.t = r
       and type Basis.t = t) (elements : (t * r) list) :
    (t, r) Stats_intf.fin_mes =
  let all_nonnegative = List.for_all (fun (_, r) -> R.(r >= zero)) elements in
  if not all_nonnegative then invalid_arg "measure: negative weight" ;
  M
    (module struct
      type nonrec t = t

      type nonrec r = r

      module V = V

      let total_weight =
        List.fold_left (fun acc (_, w) -> R.add w acc) R.zero elements

      let weightmap = V.of_list elements
    end)
  [@@inline]

let probability (type t r) (module R : Basic_intf.Reals with type t = r)
    (module V : Basic_intf.Free_module_std
      with type R.t = r
       and type Basis.t = t) (elements : (t * r) list) :
    (t, r) Stats_intf.fin_prb =
  let (_points, weights) = List.split elements in
  let total_weight = List.fold_left R.add R.zero weights in
  if not R.(total_weight = R.one) then
    Format.kasprintf
      invalid_arg
      "probability: weights do not sum up to 1 (%a)"
      R.pp
      total_weight ;
  let (M m) = measure (module R) (module V) elements in
  P m
  [@@inline]

let as_measure (Stats_intf.P prob) = Stats_intf.M prob

module Make (Reals : Basic_intf.Reals) = struct
  type r = Reals.t

  type 'a finfn = ('a, r) Stats_intf.fin_fun

  type 'a prb = ('a, r) Stats_intf.fin_prb

  type 'a mes = ('a, r) Stats_intf.fin_mes

  let measure v elements = measure (module Reals) v elements

  let probability v elements = probability (module Reals) v elements

  let total_mass (type t) (M (module D) : (t, r) Stats_intf.fin_mes) : r =
    D.total_weight

  let normalize (type t) (M (module D) : (t, r) Stats_intf.fin_mes) :
      (t, r) Stats_intf.fin_prb =
    if Reals.equal D.total_weight Reals.zero then
      invalid_arg "normalize: null measure" ;
    let imass = Reals.div Reals.one D.total_weight in
    P
      (module struct
        type t = D.t

        type nonrec r = r

        module V = D.V

        let total_weight = Reals.one

        let weightmap = V.smul imass D.weightmap
      end)

  let sample : type a. (a, r) Stats_intf.fin_mes -> Random.State.t -> a =
    fun (type t)
        (Stats_intf.M
          (module D : Stats_intf.Fin_fun with type t = t and type r = r))
        rng_state ->
     let exception Sampled of t in
     let r = Reals.(D.total_weight * lebesgue rng_state) in
     try
       let _ =
         D.V.fold
           (fun elt weight cumu ->
             let cumu = Reals.add cumu weight in
             if Reals.(r <= cumu) then raise (Sampled elt) else cumu)
           D.weightmap
           Reals.zero
       in
       (* should be unreachable *)
       assert false
     with Sampled x -> x

  let counts_of_empirical (type t)
      (module V : Basic_intf.Free_module_std
        with type R.t = Reals.t
         and type Basis.t = t) (p : t array) : (t, r) Stats_intf.fin_mes =
    let weightmap =
      Array.fold_left
        (fun vec elt -> V.add vec (V.of_list [(elt, Reals.one)]))
        V.zero
        p
    in
    M
      (module struct
        type nonrec t = t

        type nonrec r = r

        module V = V

        let total_weight = Reals.of_int (Array.length p)

        let weightmap = weightmap
      end)

  let uniform (type t)
      (module V : Basic_intf.Free_module_std
        with type R.t = Reals.t
         and type Basis.t = t) (arr : t array) : (t, r) Stats_intf.fin_prb =
    let len = Array.length arr in
    if Int.equal len 0 then failwith "uniform: empty array"
    else
      let prb = Reals.(div one (of_int len)) in
      P
        (module struct
          type nonrec t = t

          type nonrec r = r

          module V = V

          let total_weight = Reals.one

          let weightmap =
            Array.fold_left
              (fun map x -> V.add (V.of_list [(x, prb)]) map)
              V.zero
              arr
        end)

  let eval_prb (type t) (P (module P) : (t, r) Stats_intf.fin_prb) (x : t) : r =
    P.V.eval P.weightmap x

  let eval_mes (type t) (M (module P) : (t, r) Stats_intf.fin_mes) (x : t) : r =
    P.V.eval P.weightmap x

  let integrate (type t) (M (module P) : (t, r) Stats_intf.fin_mes) (f : t -> r)
      : r =
    P.V.fold (fun x w acc -> Reals.(acc + (w * f x))) P.weightmap Reals.zero

  let kernel (type a b) ?(h : (module Basic_intf.Std with type t = a) option)
      (module V : Basic_intf.Free_module_std
        with type R.t = Reals.t
         and type Basis.t = b) (kernel : a -> (b * r) list) :
      (a, b, r) Stats_intf.kernel =
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

      type nonrec r = r

      module V = V

      let kernel = kernel
    end in
    (module K)

  let compose :
      type a b c.
      ?h:(module Basic_intf.Std with type t = a) ->
      (a, b, r) Stats_intf.kernel ->
      (b, c, r) Stats_intf.kernel ->
      (a, c, r) Stats_intf.kernel =
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

      type nonrec r = r

      module V = K2.V

      let kernel = kernel
    end in
    (module Kernel)

  let constant_kernel :
      type a b. (b, r) Stats_intf.fin_prb -> (a, b, r) Stats_intf.kernel =
   fun (P (module P)) ->
    let module Kernel = struct
      type t = a

      type u = P.t

      type nonrec r = r

      module V = P.V

      let kernel _x = P.weightmap
    end in
    (module Kernel)

  let eval_kernel : type a b. a -> (a, b, r) Stats_intf.kernel -> (b * r) list =
   fun x (module K) -> K.V.fold (fun k p acc -> (k, p) :: acc) (K.kernel x) []

  let raw_data_measure (type t) (M (module D) : (t, r) Stats_intf.fin_mes) =
    let den = D.V.fold (fun elt w acc -> (elt, w) :: acc) D.weightmap [] in
    `Measure den

  let raw_data_probability (type t) (P (module D) : (t, r) Stats_intf.fin_prb) =
    let den = D.V.fold (fun elt w acc -> (elt, w) :: acc) D.weightmap [] in
    `Probability den

  let pp_fin_mes : type a. Format.formatter -> (a, r) Stats_intf.fin_mes -> unit
      =
   fun f den ->
    let (M (module D)) = den in
    let (`Measure l) = raw_data_measure den in
    Format.fprintf
      f
      "@[<h>%a@]"
      (Format.pp_print_list (fun elt_fmt (elt, pr) ->
           Format.fprintf elt_fmt "(%a, %a);@," D.V.Basis.pp elt Reals.pp pr))
      l

  let pp_fin_mes_by_measure :
      type a. Format.formatter -> (a, r) Stats_intf.fin_mes -> unit =
   fun f den ->
    let (M (module D)) = den in
    let (`Measure l) = raw_data_measure den in
    let l = List.sort (fun (_, r1) (_, r2) -> Reals.compare r1 r2) l in
    Format.fprintf
      f
      "@[<h>%a@]"
      (Format.pp_print_list (fun elt_fmt (elt, pr) ->
           Format.fprintf elt_fmt "(%a, %a);@," D.V.Basis.pp elt Reals.pp pr))
      l

  let pushforward (type t u) (prior : (t, r) Stats_intf.fin_mes)
      (likelihood : (t, u, r) Stats_intf.kernel) : (u, r) Stats_intf.fin_mes =
    let (M (module Prior)) = prior in
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

      type nonrec r = r

      module V = Likelihood.V

      let total_weight = V.fold (fun _ w acc -> Reals.add w acc) map Reals.zero

      let weightmap = map
    end in
    M (module Result)

  let inverse (type t u) ?(h : (module Basic_intf.Std with type t = u) option)
      (prior : (t, r) Stats_intf.fin_prb)
      (likelihood : (t, u, r) Stats_intf.kernel) :
      (u, r) Stats_intf.fin_prb * (u, t, r) Stats_intf.kernel =
    let (P (module Prior)) = prior in
    let (module Likelihood) = likelihood in
    let (M (module Pushforward)) = pushforward (M (module Prior)) likelihood in
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

      type nonrec r = r

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
    (P (module Pushforward), (module Kernel))

  module Bool_vec =
    Basic_impl.Free_module.Make
      (struct
        include Bool

        let pp = Format.pp_print_bool

        let hash = Hashtbl.hash
      end)
      (Reals)
      (Map.Make (Bool))

  module Int_vec =
    Basic_impl.Free_module.Make (Std.Int) (Reals) (Basic_impl.Int_map)

  let coin ~bias : (bool, r) Stats_intf.fin_prb =
    if Reals.(bias < zero || bias > one) then invalid_arg "coin: invalid bias"
    else
      probability (module Bool_vec) [(true, bias); (false, Reals.(one - bias))]

  let bincoeff n k =
    let n = Reals.of_int n in
    let rec loop i acc =
      if Int.equal i (k + 1) then acc
      else
        let fi = Reals.of_int i in
        loop (i + 1) Reals.(acc * ((n + one - fi) / fi))
    in
    loop 1 Reals.one

  let binomial (coin : (bool, r) Stats_intf.fin_prb) n =
    let p = eval_prb coin true in
    let not_p = eval_prb coin false in
    let elements =
      List.init n (fun k ->
          let n_minus_k = n - k in
          Reals.(k, bincoeff n k * npow p k * npow not_p n_minus_k))
    in
    normalize @@ measure (module Int_vec) elements

  let mean_generic (type elt)
      (module L : Basic_intf.Module with type t = elt and type R.t = Reals.t)
      (M (module Dist) : (elt, r) Stats_intf.fin_mes) =
    Dist.V.fold (fun x w acc -> L.add (L.smul w x) acc) Dist.weightmap L.zero

  let mean (dist : (r, r) Stats_intf.fin_mes) = integrate dist (fun x -> x)

  let variance (dist : (r, r) Stats_intf.fin_mes) =
    let (M (module Dist)) = dist in
    let m = mean dist in
    Dist.V.fold
      (fun x w acc ->
        let acc =
          let open Reals in
          let delta = x - m in
          let delta_squared = delta * delta in
          acc + (delta_squared * w)
        in
        acc)
      Dist.weightmap
      Reals.zero

  let fold_union (type x) f (m1 : x mes) (m2 : x mes) acc =
    let (M (module Dist1)) = m1 in
    let (M (module Dist2)) = m2 in
    let module Table = Hashtbl.Make (Dist1.V.Basis) in
    let table = Table.create 127 in
    Dist1.V.fold (fun x r () -> Table.add table x r) Dist1.weightmap () ;
    let acc =
      Dist2.V.fold
        (fun y r acc ->
          match Table.find_opt table y with
          | None -> f y Reals.zero r acc
          | Some r' ->
              Table.remove table y ;
              f y r r' acc)
        Dist2.weightmap
        acc
    in
    (* process elements in support(m1)\support(m2) *)
    Table.fold (fun x r acc -> f x r Reals.zero acc) table acc
end
[@@inline]

module Float = struct
  include Make (Basic_impl.Reals.Float)

  module Dist = struct
    let kl m1 m2 =
      fold_union (fun _ r1 r2 acc -> acc +. (r1 *. log (r1 /. r2))) m1 m2 0.0

    let lp ~p m1 m2 =
      if p <. 1. then invalid_arg "lp: p < 1" ;
      let res =
        fold_union
          (fun _ r1 r2 acc -> acc +. (abs_float (r1 -. r2) ** p))
          m1
          m2
          0.0
      in
      res ** (1. /. p)

    let maxf x y = if x <. y then y else x

    let linf m1 m2 =
      fold_union (fun _ r1 r2 acc -> maxf acc (abs_float (r1 -. r2))) m1 m2 0.0
  end
end

module Rational = struct
  include Make (Basic_impl.Reals.Rational)

  module Dist = struct
    let maxq x y = if Q.(x < y) then y else x

    let linf m1 m2 =
      fold_union
        (fun _ r1 r2 acc -> maxq acc (Q.abs (Q.sub r1 r2)))
        m1
        m2
        Q.zero
  end
end
