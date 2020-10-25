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

module Brusselator (Op : Fadbad.OpS with type scalar = float) = struct
  let a = 1.
  let b = 1.7

  let exec s _ =
    let x = Array.get s 0 in
    let y = Array.get s 1 in
    let open Op in
    let xxy = x * x * y in
    let bx = scale x b in
    let dx = translate (xxy - bx - x) a in
    let dy = bx - xxy in
    [| dx; dy |]
end

module Experiment (Func : Fode.S) (Op : Sets.S) = struct
  module Integrator = ContinuousIntegrator.Make(Func)(Op)
  module TM = TaylorModel.Make(Op)

  let display_threshold = ref 3e-3

  let to_interval x =
    let min, max = Op.get_min_max x in
    Interval.make_bounds min max

  let print_intervals s dimX dimY =
    let x = Array.get s dimX in
    let y = Array.get s dimY in
    Interval.print2d (to_interval x) (to_interval y)

  let print_state s dimX dimY =
    let x = Array.get s dimX in
    let y = Array.get s dimY in
    Op.print2d x y !display_threshold

  let print l =
    let rec print printer l =
      match l with
      | [] -> ()
      | h::t ->
         let () = printer h 0 1 in
         let () = Printf.printf "\n" in
         print printer t
    in
    print print_state l

  let to_op l dt =
    let rec to_op r l =
      match l with
      | [] -> r
      | h::t ->
         let s = TM.eval h dt in
         to_op (s::r) t
    in
    to_op [] l


  let run s0 t0 t_end dt order threshold =
    let () = display_threshold := threshold in
    let () = Integrator.set_dt dt in
    let () = Integrator.set_order order in
    let l = Integrator.integrate s0 t0 t_end in
    let () = Printf.fprintf Stdlib.stderr "Print...\n%!" in
    let instants = (to_op l (Op.make_float 0.)) in
    let () = print instants in
    let () = Printf.printf "\n" in
    let flowpipes = to_op l (Op.make_bounds 0. dt) in
    let () = print flowpipes in
    ()
end


let () =
  let module Op = Sets.AffineForm in
  let module Exp = Experiment(Brusselator)(Op) in
  let s0 = [| Op.make_bounds 1.45 1.55; Op.make_bounds 2.95 3.05 |] in
  let t0 = 0. in
  let tEnd = ref 2. in
  let dt = ref 2e-2 in
  let order = ref 3 in
  let display_threshold = ref 3e-3 in

  Arg.(parse [
           "-tend", Set_float tEnd, "final time";
           "-dt", Set_float dt, "time step";
           "-order", Set_int order, "Taylor model order";
           "-threshold", Set_float display_threshold, "noises threshold to display zonotopes";
  ]) (fun s -> ()) "./exampleReachability [-tend tEnd] [-dt dt] [-order order]";

  let () = Exp.run s0 t0 !tEnd !dt !order !display_threshold in
  ()
