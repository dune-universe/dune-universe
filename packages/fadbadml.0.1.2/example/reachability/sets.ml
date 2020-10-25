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

module type S = sig
  include Fadbad.OpS with type scalar = float

  val make_float: float -> t
  val make_bounds: float -> float -> t
  val get_min_max: t -> (float * float)
  val print2d: t -> t -> float -> unit

  val radius: t -> float
end

module Interval = Interval
module AffineForm = AffineForm

type t =
  | Interval of Interval.t
  | AAF of AffineForm.t

let make_interval i = Interval(i)
let make_aaf aaf = AAF(aaf)

let to_affine_form s =
  match s with
  | Interval(i) -> AffineForm.make_bounds i.min i.max
  | AAF(aaf) -> aaf

let to_interval s =
  match s with
  | Interval(i) -> i
  | AAF(aaf) ->
       let radius = AffineForm.radius aaf in
       let center = aaf.t_center in
       Interval.make_bounds (center -. radius) (center +. radius)
