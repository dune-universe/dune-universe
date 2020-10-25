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

module OpFloat = Fadbad.OpFloat
module BOp = Fadbad.B(OpFloat)
module TOp = Fadbad.T(OpFloat)

module Brusselator (Op : Fadbad.OpS with type scalar = float and type elt = float) =
struct
  type vec = { x : Op.t; y : Op.t }

  let zero () = { x = Op.zero (); y = Op.zero () }
  let ( ++ ) a b = let open Op in { x = a.x + b.x; y = a.y + b.y }
  let scale_v v a = let open Op in { x = scale v.x a; y = scale v.y a }

  type memory = {
    mutable lastv : vec;
    mutable initial : bool;
  }

  type 'a node = {
    alloc : unit -> memory;
    reset : memory -> unit;
    step : memory -> 'a;
  }

  let make_euler v0 f =
    let alloc () = { lastv = zero (); initial = true } in
    let reset mem = mem.lastv <- zero (); mem.initial <- true in
    let step mem dt =
      let res =
        if mem.initial then begin mem.initial <- false; v0 end
        else mem.lastv ++ (scale_v (f mem.lastv) dt)
      in
      mem.lastv <- res;
      res
    in { alloc; reset; step }

  let brusselator v =
    let open Op in
    {
      x = translate ((sqr v.x) * v.y - (scale v.x 2.7)) 1.;
      y = (scale v.x 1.7) - (sqr v.x) * v.y;
    }

  let fd = ref None
  let get_fd () = match !fd with None -> assert false | Some fd -> fd

  let init_write filename =
    fd := Some (open_out filename); Printf.fprintf (get_fd()) "t\tx\ty\n"
  let write t v =
    Printf.fprintf (get_fd ()) "%f\t%f\t%f\n" t (Op.get v.x) (Op.get v.y)
  let close_write () = close_out (get_fd ()); fd := None
end

type bad_values = {
  bad_t : float;
  bad_x : float;
  bad_y : float;
  bad_dxdx0 : float;
  bad_dydx0 : float;
  bad_dxdy0 : float;
  bad_dydy0 : float;
}

type tad_values = {
  tad_t : float;
  tad_x : float;
  tad_y : float;
  tad_dxdt : float array;
  tad_dydt : float array;
}

type 'a result = {
  exec_time : float;
  dt : float;
  nsteps : int;
  values : 'a;
}

let print_float name ff f = Format.fprintf ff "@[<h>%s@ =@ %f;@]" name f
let print_int name ff i = Format.fprintf ff "@[<h>%s@ =@ %d;@]" name i

let print_float_array name ff a =
  Format.fprintf ff "@[<h>%s@ =@ [%s];@]" name
    (String.concat "; " (Array.to_list (Array.map string_of_float a)))

let print_bad_values ff values =
  Format.fprintf ff "@[<v 2>{@;%a@;%a@;%a@;%a@;%a@;%a@;%a@]@;}"
    (print_float "t") values.bad_t
    (print_float "x") values.bad_x
    (print_float "y") values.bad_y
    (print_float "dx/dx0") values.bad_dxdx0
    (print_float "dy/dx0") values.bad_dydx0
    (print_float "dx/dy0") values.bad_dxdy0
    (print_float "dy/dy0") values.bad_dydy0

let print_tad_values ff values =
  Format.fprintf ff "@[<v 2>{@;%a@;%a@;%a@;%a@;%a@]@;}"
    (print_float "t") values.tad_t
    (print_float "x") values.tad_x
    (print_float "y") values.tad_y
    (print_float_array "dx/dt") values.tad_dxdt
    (print_float_array "dy/dt") values.tad_dydt

let print_res f_values ff res =
  Format.printf "@[<v 2>{@;%a@;%a@;%a@;@[<h>values@ =@ %a;@]@]@;}"
    (print_float "exec_time") res.exec_time
    (print_float "dt") res.dt
    (print_int "nsteps") res.nsteps
    f_values res.values

let print_bad_res = print_res print_bad_values
let print_tad_res = print_res print_tad_values

let main_bad nsteps dt filename =
  let open Brusselator(BOp) in
  let v0 = { x = BOp.one (); y = BOp.one () } in
  let { alloc; step; reset } = make_euler v0 brusselator in
  let mem = alloc () in reset mem;
  let t = ref 0. in
  if filename <> "" then init_write filename;
  for i = 0 to nsteps do
    let v = step mem dt in
    if filename <> "" then write !t v;
    t := !t +. dt;
  done;
  if filename <> "" then close_write ();
  BOp.diff mem.lastv.x 0 2;
  BOp.diff mem.lastv.y 1 2;
  let exec_t = Unix.gettimeofday () in
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
      bad_dydx0 = BOp.d v0.y 0;
      bad_dxdy0 = BOp.d v0.x 1;
      bad_dydy0 = BOp.d v0.y 1;
    }
  }

let main_tad nsteps dt ncoeff =
  let open Brusselator(TOp) in
  let v0 = { x = TOp.one (); y = TOp.one () } in
  let { alloc; step; reset } = make_euler v0 brusselator in
  let mem = alloc () in reset mem;
  let t = ref 0. in
  for i = 0 to nsteps do
    ignore (step mem dt);
    t := !t +. dt;
  done;

  TOp.set v0.x 1 (OpFloat.one ());
  TOp.set v0.y 1 (OpFloat.one ());

  let exec_t = Unix.gettimeofday () in
  ignore (TOp.eval mem.lastv.x ncoeff);
  ignore (TOp.eval mem.lastv.y ncoeff);
  let cur_exec_time = Unix.gettimeofday () -. exec_t in

  {
    exec_time = cur_exec_time;
    dt; nsteps;
    values = {
      tad_t = !t;
      tad_x = TOp.get mem.lastv.x;
      tad_y = TOp.get mem.lastv.y;
      tad_dxdt = TOp.get_derivatives mem.lastv.x;
      tad_dydt = TOp.get_derivatives mem.lastv.y;
    }
  }



let _ =
  let nsteps = ref 10 in
  let dt = ref 0.001 in
  let filename = ref "" in
  let ntcoeff = ref 2 in
  let nobad = ref false in
  let notad = ref false in

  Arg.(parse [
    "-n", Set_int nsteps, "number of steps";
    "-dt", Set_float dt, "timestep";
    "-o", Set_string filename, "output file name";
    "-ntcoeff", Set_int ntcoeff, "number of taylor coefficients to compute";
    "-nobad", Set nobad, "do not compute the derivatives using the backward method";
    "-notad", Set notad, "do not compute taylor coefficients";
  ]) (fun s -> ()) "./exampleBrusselator [-n N] [-dt DT] [-o LOGFILE]";

  if not !nobad then
    let res = main_bad !nsteps !dt !filename in
    Format.printf "BAD: %a@." print_bad_res res;

  if not !notad then
    let res = main_tad !nsteps !dt !ntcoeff in
    Format.printf "TAD: %a@." print_tad_res res;

  ()
