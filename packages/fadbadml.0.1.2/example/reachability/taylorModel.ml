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

module Make (Op : Fadbad.OpS) = struct
  type t =
    {
      dim: int; (* space dimension *)
      coef: (Op.t array) list; (* [a_{n}, a_{n-1}, ..., a_{0}] *)
      t0: Op.t;
      dt_max: Op.t;
    }

  let eval tm dt =
    let open Op in
    let rec aux r l =
      match l with
      | [] -> r
      | h::t ->
         let r = Array.map (fun x -> dt * x) r in
         let r = Array.map2 (fun x h -> x + h) r h in
         aux r t
    in
    aux (Array.make tm.dim (zero ())) tm.coef
end
