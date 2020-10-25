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

let default_nsteps = 10
let default_dt = 0.001
let default_ncoeff = 5

type 'a result = {
  exec_time : float;
  dt : float;
  nsteps : int;
  values : 'a;
}

let print_float name ff f = Format.fprintf ff "@[<h>\"%s\":@ %f@]" name f
let print_int name ff i = Format.fprintf ff "@[<h>\"%s\":@ %d@]" name i

let print_float_array name ff a =
  Format.fprintf ff "@[<h>\"%s\":@ [%s]@]" name
    (String.concat ", " (Array.to_list (Array.map (fun f -> Printf.sprintf "%f" f) a)))

let print_res f_values ff res =
  Format.fprintf ff "@[<v 2>{@;%a,@;%a,@;%a,@;@[<h>\"values\":@ %a@]@]@;}"
    (print_float "exec_time") res.exec_time
    (print_float "dt") res.dt
    (print_int "nsteps") res.nsteps
    f_values res.values
