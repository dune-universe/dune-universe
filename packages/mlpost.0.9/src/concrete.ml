(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

let supported = true

type cnum = float

let set_verbosity = Defaults.set_verbosity

let set_prelude = Defaults.set_prelude_from_file

let set_prelude2 s =
  let s = match s with None -> "" | Some s -> s in
  Defaults.set_prelude s

let set_t1disasm = Defaults.set_t1disasm

module CPoint = Point_lib

module CPath = struct
  module S = Spline_lib

  type t = S.path

  type abscissa = float

  let length p = float (S.length p)

  let is_closed = S.is_closed

  let is_a_point x = S.is_a_point x

  let intersection p1 p2 = S.intersection p1 p2

  let one_intersection p1 p2 = S.one_intersection p1 p2

  let reverse = S.reverse

  let iter = S.iter

  let fold_left = S.fold_left

  let cut_before = S.cut_before

  let cut_after = S.cut_after

  let split = S.split

  let subpath = S.subpath

  let direction_of_abscissa = S.direction_of_abscissa

  let point_of_abscissa = S.abscissa_to_point

  let bounding_box = S.bounding_box

  let dist_min_point = S.dist_min_point

  let dist_min_path = S.dist_min_path

  let print = S.print
end

module CTransform = Matrix

let float_of_num = Compute.num

let cpoint_of_point = Compute.point

let cpath_of_path = Compute.path

let ctransform_of_transform = Compute.transform

let baselines s = Picture_lib.baseline (Compute.picture (Types.mkPITex s))

let num_of_float = Misc.id

let point_of_cpoint = Misc.id

let path_of_cpath p =
  let knot x = Types.mkKnot Types.mkNoDir (point_of_cpoint x) Types.mkNoDir in
  let start = knot (CPath.point_of_abscissa p 0.) in
  let path =
    CPath.fold_left
      (fun acc _ b c d ->
        let joint = Types.mkJControls (point_of_cpoint b) (point_of_cpoint c) in
        Types.mkMPAConcat (knot d) joint acc)
      (Types.mkMPAKnot start) p
  in
  if CPath.is_closed p then Types.mkMPACycle Types.mkNoDir Types.mkJLine path
  else Types.mkPAofMPA path

let transform_of_ctransform x = [ x ]
