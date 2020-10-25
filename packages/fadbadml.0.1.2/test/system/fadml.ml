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

open Common

type fad_values = {
  fad_t : float;
  fad_x : float;
  fad_y : float;
  fad_dxdx0 : float;
  fad_dydx0 : float;
  fad_dxdy0 : float;
  fad_dydy0 : float;
}

let print_fad_values ff values =
  Format.fprintf ff "@[<v 2>{@;%a,@;%a,@;%a,@;%a,@;%a,@;%a,@;%a@]@;}"
    (print_float "t") values.fad_t
    (print_float "x") values.fad_x
    (print_float "y") values.fad_y
    (print_float "dx/dx0") values.fad_dxdx0
    (print_float "dy/dx0") values.fad_dydx0
    (print_float "dx/dy0") values.fad_dxdy0
    (print_float "dy/dy0") values.fad_dydy0

let print_fad_res = print_res print_fad_values

module FOp = Fadbad.F(Fadbad.OpFloat)

let main_fad nsteps dt =
  let open Brusselator.Make(FOp) in
  let v0 = { x = FOp.one (); y = FOp.one () } in
  let { alloc; step; reset } = make_euler v0 brusselator in
  let t = ref 0. in

  FOp.diff v0.x 0 2;
  FOp.diff v0.y 1 2;
  let mem = alloc () in reset mem;
  let exec_t = Unix.gettimeofday () in
  ignore (step mem dt); (* initialization *)
  (* start loop *)
  for _ = 1 to nsteps do
    ignore (step mem dt);
    t := !t +. dt;
  done;
  (* end loop *)
  let cur_exec_time = Unix.gettimeofday () -. exec_t in

  {
    exec_time = cur_exec_time;
    dt; nsteps;
    values = {
      fad_t = !t;
      fad_x = FOp.get mem.lastv.x;
      fad_y = FOp.get mem.lastv.y;
      fad_dxdx0 = FOp.d mem.lastv.x 0;
      fad_dydx0 = FOp.d mem.lastv.y 0;
      fad_dxdy0 = FOp.d mem.lastv.x 1;
      fad_dydy0 = FOp.d mem.lastv.y 1;
    }
  }

let _ =
  let nsteps = ref default_nsteps in
  let dt = ref default_dt in

  Arg.(parse [
    "-n", Set_int nsteps,
      Printf.sprintf "number of steps to compute (default: %d)" !nsteps;
    "-dt", Set_float dt,
      Printf.sprintf "size of one step (default: %f)" !dt;
  ]) (fun _ -> ()) "usage: ./fad_ml [[-]-help] [-n N] [-dt DT]\n";

  let res = main_fad !nsteps !dt in
  Format.printf "%a@." print_fad_res res
