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

type bad_values = {
  bad_t : float;
  bad_x : float;
  bad_y : float;
  bad_dxdx0 : float;
  bad_dydx0 : float;
  bad_dxdy0 : float;
  bad_dydy0 : float;
}

let print_bad_values ff values =
  Format.fprintf ff "@[<v 2>{@;%a,@;%a,@;%a,@;%a,@;%a,@;%a,@;%a@]@;}"
    (print_float "t") values.bad_t
    (print_float "x") values.bad_x
    (print_float "y") values.bad_y
    (print_float "dx/dx0") values.bad_dxdx0
    (print_float "dy/dx0") values.bad_dydx0
    (print_float "dx/dy0") values.bad_dxdy0
    (print_float "dy/dy0") values.bad_dydy0

let print_bad_res = print_res print_bad_values

module BOp = Fadbad.B(Fadbad.OpFloat)

let main_bad nsteps dt =
  let open Brusselator.Make(BOp) in
  let v0 = { x = BOp.one (); y = BOp.one () } in
  let { alloc; step; reset } = make_euler v0 brusselator in
  let t = ref 0. in

  let mem = alloc () in reset mem;
  let exec_t = Unix.gettimeofday () in
  ignore (step mem dt); (* initialization *)
  (* start loop *)
  for _ = 1 to nsteps do
    ignore (step mem dt);
    t := !t +. dt;
  done;
  (* end loop *)
  BOp.diff mem.lastv.x 0 2;
  BOp.diff mem.lastv.y 1 2;
  BOp.compute_list [mem.lastv.x; mem.lastv.y];
  let cur_exec_time = Unix.gettimeofday () -. exec_t in

  {
    exec_time = cur_exec_time;
    dt; nsteps;
    values = {
      bad_t = !t;
      bad_x = BOp.get mem.lastv.x;
      bad_y = BOp.get mem.lastv.y;
      bad_dxdx0 = BOp.d v0.x 0;
      bad_dydx0 = BOp.d v0.x 1;
      bad_dxdy0 = BOp.d v0.y 0;
      bad_dydy0 = BOp.d v0.y 1;
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
  ]) (fun _ -> ()) "usage: ./bad_ml [[-]-help] [-n N] [-dt DT]\n";

  let res = main_bad !nsteps !dt in
  Format.printf "%a@." print_bad_res res
