(**************************************************************************)
(*                                                                        *)
(*                                FADBADml                                *)
(*                                                                        *)
(*           OCaml port by François Bidet and Ismail Bennani              *)
(*     Based on FADBAD++, written by Ole Stauning and Claus Bendtsen      *)
(*                                                                        *)
(*                          Copyright 2019-2020                           *)
(*                                                                        *)
(*   This file is distributed under the terms of the CeCILL-C license.    *)
(*                                                                        *)
(**************************************************************************)

module Make (Func : Fode.S) (Op : Sets.S) = struct
  module FuncInterval = Func(Interval)
  module T = Fadbad.T(Op)
  module FuncT = Func(T)
  module TM = TaylorModel.Make(Op)

  let dt = ref 1e-1
  let order = ref 3

  let margin = 0.1
  let dilation = 1.1

  let set_dt v =
    dt := v

  let set_order o =
    order := o

  let for_all2 p s1 s2 =
    let dim = Array.length s1 in
    let rec test i =
      if i >= dim then
        true
      else if p s1.(i) s2.(i) then
        test (i+1)
      else
        false
    in
    test 0


  let add_margin s m =
    let open Interval in
    let margin  = make_bounds (-.m) m in
    Array.map (fun i -> i + margin) s

  let dilate s a =
    let open Interval in
    Array.map
      (fun i ->
        let rad = radius i in
        let margin = make_bounds (-.rad) rad in
        let margin = scale margin a in
        i + margin)
      s

  let scale s a =
    let open Interval in
    Array.map (fun i ->  i * a) s

  let add s1 s2 =
    let open Interval in
    Array.map2 (fun i1 i2 -> i1 + i2) s1 s2

  let compute_enclosure s0 t0 =
    let s0 = Array.map
               (fun s ->
                 let min,max = Op.get_min_max s in
                 Interval.make_bounds min max)
               s0
    in
    let deltaT = Interval.make_bounds 0. !dt in
    let compute_next enc =
      add s0 (scale (FuncInterval.exec enc deltaT) deltaT)
    in
    let rec find_fix_point enc next =
      if for_all2 Interval.subset next enc then
        next
      else
        let enc = dilate enc dilation in
        let next = compute_next enc in
        find_fix_point enc next
    in
    let enc = add_margin s0 0.1 in (* ensure no punctual state *)
    let next_enc = compute_next enc in
    find_fix_point enc next_enc

  let compute_expansion s0 t0 order =
    let x = Array.map (fun s -> T.lift s) s0 in
    let xp = FuncT.exec x t0 in
    let dim = Array.length x in
    (* eval all dimensions and store result in [r] *)
    let rec aux_dim r order d =
      if d >= dim then
        r
      else
        let _ = T.eval xp.(d) order in
        let () =
          T.set
            x.(d)
            (order+1)
            (Op.scale (T.deriv xp.(d) order) (1. /. (float (order + 1))))
        in
        let () = Array.set r d (T.deriv x.(d) (order+1)) in
        aux_dim r order (d+1)
    in
    (* eval all order and store expansion in [r] *)
    let rec aux_order r o =
      if o > order then
        r
      else
        let alpha = Array.make dim (Op.zero ()) in
        let alpha = aux_dim alpha o 0 in
        aux_order (alpha::r) (o+1)
    in
    aux_order [s0] 0

  let step s0 t0 : TM.t =
    let t0T = T.lift t0 in
    let enc = compute_enclosure s0 t0T in
    (* Taylor model coefficients *)
    let coef = compute_expansion s0 t0T (!order-1) in
    (* Taylor model remainder *)
    let enc = Array.map
                (fun i ->
                  let min, max = Interval.get_min_max i in
                  Op.make_bounds min max)
                enc
    in
    let enc_coef = compute_expansion enc t0T !order in
    let remainder =
      match enc_coef with
      | [] -> assert false
      | h::_ -> h
    in
    let coef = remainder :: coef in
    {
      dim = Array.length s0;
      coef;
      t0;
      dt_max = Op.make_float !dt;
    }

  (* return list of Taylor models at each step *)
  let integrate s0 t0 tEnd =
    let dtOp = Op.make_float !dt in
    let rec aux r s t =
      if t > tEnd then
        r
      else
        let () = Printf.fprintf Stdlib.stderr "time: %s\n%!" (string_of_float t) in
        let tm = step s (Op.make_float t) in
        (* let () = print_endline (string_of_int (List.length tm.coef)) in *)
        let next_s = TM.eval tm dtOp in
        let next_t = t +. !dt in
        aux (tm :: r) next_s next_t
    in
    aux [] s0 t0
end
