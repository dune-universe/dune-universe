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

module type OpFloatS = Fadbad.OpS with type elt = float and type scalar = float

type 'a dprice =
  {
    price:        'a;
    d_price_spot: 'a;
    d_price_vol:  'a;
    d_price_r:    'a;
  }

type 'a ddprice =
  {
    price:          'a;
    d_price_spot:   'a;
    d_price_vol:    'a;
    d_price_r:      'a;
    d_price_2_spot: 'a;
  }

let get_one_gaussian_by_summation () =
  let rec sum_rand acc n =
    if n = 0 then
      acc
    else
      sum_rand (acc +. (Random.float 1.)) (n - 1)
  in
  (sum_rand 0. 12) -. 6.


let get_one_gaussian_by_Box_Muller () =
  let rec aux x y =
    let size_squared = (x *. x) +. (y *. y) in
    if size_squared < 1. then
      x *. sqrt( -2. *. (log size_squared) /. size_squared)
    else
      let x = (2. *. (Random.float 1.)) -. 1. in
      let y = (2. *. (Random.float 1.)) -. 1. in
      aux x y
  in
  aux 10. 10.

module SimpleMonteCarlo (Op : OpFloatS) =
  struct
    let compute expiry strike spot vol r nb_paths =
      let open Op in
      let variance = scale (vol * vol) expiry in
      let root_variance = sqrt variance in
      let ito_correction = scale variance (-0.5) in
      let moved_spot = spot * (exp ((scale r expiry) + ito_correction)) in
      let rec aux sum nb =
        if Stdlib.(nb <= 0) then
          sum
        else
          let this_gaussian = get_one_gaussian_by_Box_Muller () in
          let this_spot = moved_spot * (exp (scale root_variance this_gaussian)) in
          let this_payoff = translate this_spot (-.strike) in
          let this_payoff =
            if Stdlib.((Op.get this_payoff) > 0.) then
              this_payoff
            else
              Op.zero ()
          in
          aux (sum + this_payoff) Stdlib.(nb - 1)
      in
      let mean = scale (aux (zero ()) nb_paths) (1. /. (float nb_paths)) in
      mean * (exp (scale r (-. expiry)))
  end

module BADSimpleMonteCarlo (Op : OpFloatS) =
  struct
    module BOp = Fadbad.B(Op)
    module SMC = SimpleMonteCarlo(BOp)

    let compute expiry strike spot vol r nb_paths =
      let open BOp in
      let b_spot = lift spot in
      let b_vol = lift vol in
      let b_r = lift r in
      let b_price = SMC.compute expiry strike b_spot b_vol b_r nb_paths in
      let () = diff b_price 0 1 in
      let () = compute b_price in
      {
        price = value b_price;
        d_price_spot = deriv b_spot 0;
        d_price_vol = deriv b_vol 0;
        d_price_r = deriv b_r 0;
      }

    let print (r : Op.t dprice) =
      let open Op in
      Printf.printf
        "BAD:\nPrice = %f\ndPriceSpot = %f\ndPriceVol = %f\ndPriceR = %f\n\n"
        (get r.price) (get r.d_price_spot) (get r.d_price_vol) (get r.d_price_r)
  end

module FADSimpleMonteCarlo (Op : OpFloatS) =
  struct
    module FOp = Fadbad.F(Op)
    module SMC = SimpleMonteCarlo(FOp)

    let compute expiry strike spot vol r nb_paths =
      let open FOp in
      let f_spot = lift spot in
      let f_vol = lift vol in
      let f_r = lift r in
      let () = diff f_spot 0 3 in
      let () = diff f_vol 1 3 in
      let () = diff f_r 2 3 in
      let f_price = SMC.compute expiry strike f_spot f_vol f_r nb_paths in
      {
        price = value f_price;
        d_price_spot = deriv f_price 0;
        d_price_vol = deriv f_price 1;
        d_price_r = deriv f_price 2;
      }

    let print (r : Op.t dprice) =
      let open Op in
      Printf.printf
        "FAD:\nPrice = %f\ndPriceSpot = %f\ndPriceVol = %f\ndPriceR = %f\n\n"
        (get r.price) (get r.d_price_spot) (get r.d_price_vol) (get r.d_price_r)
  end


module FADFADSimpleMonteCarlo (Op : OpFloatS) =
  struct
    module FOp = Fadbad.F(Op)
    module FSMC = FADSimpleMonteCarlo(FOp)

    let compute expiry strike spot vol r nb_paths =
      let open FOp in
      let f_spot = lift spot in
      let f_vol = lift vol in
      let f_r = lift r in
      let () = diff f_spot 0 1 in
      let f = FSMC.compute expiry strike f_spot f_vol f_r nb_paths in
      {
        price = value f.price;
        d_price_spot = value f.d_price_spot;
        d_price_vol = value f.d_price_vol;
        d_price_r = value f.d_price_r;
        d_price_2_spot = deriv f.d_price_spot 0;
      }

    let print r =
      let open Op in
      Printf.printf
        "FADFAD:\nPrice = %f\ndPriceSpot = %f\ndPriceVol = %f\ndPriceR = %f\ndPrice2Spot = %f\n\n"
        (get r.price) (get r.d_price_spot) (get r.d_price_vol) (get r.d_price_r)
        (get r.d_price_2_spot)
  end


module FADBADSimpleMonteCarlo (Op : OpFloatS) =
  struct
    module FOp = Fadbad.F(Op)
    module BSMC = BADSimpleMonteCarlo(FOp)

    let compute expiry strike spot vol r nb_paths =
      let open FOp in
      let f_spot = lift spot in
      let f_vol = lift vol in
      let f_r = lift r in
      let () = diff f_spot 0 1 in
      let f = BSMC.compute expiry strike f_spot f_vol f_r nb_paths in
      {
        price = value f.price;
        d_price_spot = value f.d_price_spot;
        d_price_vol = value f.d_price_vol;
        d_price_r = value f.d_price_r;
        d_price_2_spot = deriv f.d_price_spot 0;
      }

    let print r =
      let open Op in
      Printf.printf
        "FADBAD:\nPrice = %f\ndPriceSpot = %f\ndPriceVol = %f\ndPriceR = %f\ndPrice2Spot = %f\n\n"
        (get r.price) (get r.d_price_spot) (get r.d_price_vol) (get r.d_price_r)
        (get r.d_price_2_spot)
  end


let () =
  let module OpFloat = Fadbad.OpFloat in
  let module FAD = FADSimpleMonteCarlo(OpFloat) in
  let module BAD = BADSimpleMonteCarlo(OpFloat) in
  let module FADFAD = FADFADSimpleMonteCarlo(OpFloat) in
  let module FADBAD = FADBADSimpleMonteCarlo(OpFloat) in

  let strike = 1.3 in
  let expiry = 1.0 in
  let spot = OpFloat.make 1.3 in
  let vol = OpFloat.make 0.08 in
  let r = OpFloat.make 0.06 in
  let nb_paths = 1000 in

  let () = Random.init 0 in
  let () = FAD.print (FAD.compute expiry strike spot vol r nb_paths) in

  let () = Random.init 0 in
  let () = BAD.print (BAD.compute expiry strike spot vol r nb_paths) in

  let () = Random.init 0 in
  let () = FADFAD.print (FADFAD.compute expiry strike spot vol r nb_paths) in

  let () = Random.init 0 in
  let () = FADFAD.print (FADFAD.compute expiry strike spot vol r nb_paths) in

  let () = Random.init 0 in
  let () = FADBAD.print (FADBAD.compute expiry strike spot vol r nb_paths) in

  ()
