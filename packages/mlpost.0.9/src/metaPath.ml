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

module S = Point
open Types

module BaseDefs = struct
  type direction = Types.direction

  let vec = mkVec

  let curl = mkCurl

  let noDir = mkNoDir

  type joint = Types.joint

  let jLine = mkJLine

  let jCurve = mkJCurve

  let jCurveNoInflex = mkJCurveNoInflex

  let jTension = mkJTension

  let jControls = mkJControls

  type knot = Types.knot

  (* the intention is to add new knots in front,
   * i. e. the list has to be reversed for printing *)

  let of_path p = mkMPAofPA p

  let of_metapath p = mkPAofMPA p

  let to_path = of_metapath

  let to_metapath = of_path

  let start k = mkMPAKnot k

  let metacycle d j p = mkMPACycle d j p

  let fullcircle = mkPAFullCircle

  let halfcircle = mkPAHalfCircle

  let quartercircle = mkPAQuarterCircle

  let unitsquare = mkPAUnitSquare

  let cut_after p1 p2 = mkPACutAfter p1 p2

  let cut_before p1 p2 = mkPACutBefore p1 p2

  let build_cycle l = mkPABuildCycle l

  let subpath (f1 : float) (f2 : float) p = mkPASub f1 f2 p

  (*   let point (f: float) p = mkPTPointOf f p *)
  (*   let direction (f: float) p = mkPTDirectionOf f p *)

  let subpathn = subpath

  let length p =
    let p = Compute.path p in
    float (Spline_lib.length p)

  let defaultjoint = jCurve

  let defaultdir = noDir
end

include BaseDefs

type t = metapath

type path = Types.path

let knotp ?(l = defaultdir) ?(r = defaultdir) p = Types.mkKnot l p r

let knot ?l ?r ?scale p = knotp ?l (S.p ?scale p) ?r

let knotn ?l ?r p = knotp ?l (S.pt p) ?r

let knotlist = List.map (fun (x, y, z) -> Types.mkKnot x y z)

let cycle ?(dir = defaultdir) ?(style = defaultjoint) p = metacycle dir style p

let concat ?(style = defaultjoint) p k = mkMPAConcat k style p

(* construct a path with a given style from a knot list *)
let pathk ?style = function
  | [] -> failwith "empty path is not allowed"
  | x :: xs -> List.fold_left (fun p knot -> concat ?style p knot) (start x) xs

let pathp ?style l = pathk ?style (List.map knotp l)

let pathn ?style l = pathp ?style (List.map Point.pt l)

let path ?style ?scale l =
  let sc = S.ptlist ?scale in
  pathp ?style (sc l)

(* construct a path with knot list and joint list *)
let jointpathk lp lj =
  try
    List.fold_left2
      (fun acc j k -> mkMPAConcat k j acc)
      (start (List.hd lp))
      lj (List.tl lp)
  with Invalid_argument _ ->
    invalid_arg
      "jointpathk : the list of knot must be one more than the list of join"

let jointpathp lp lj = jointpathk (List.map knotp lp) lj

let jointpathn lp lj = jointpathk (List.map knotn lp) lj

let jointpath ?scale lp lj = jointpathk (List.map (knot ?scale) lp) lj

let append ?(style = defaultjoint) p1 p2 = mkMPAAppend p1 style p2
