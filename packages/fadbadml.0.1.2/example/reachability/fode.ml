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

(**
 * First order ordinary differential equation `f`:
 * x' = f(x,t)
 *)

module type S =
  functor (Op : Fadbad.OpS with type scalar = float) ->
  sig
    (** [exec x t] eval derivative at state [x] and time [t] *)
    val exec: Op.t array -> Op.t -> Op.t array
  end
