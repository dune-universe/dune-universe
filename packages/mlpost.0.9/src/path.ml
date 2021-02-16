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

open MetaPath
open Types
include MetaPath.BaseDefs

let transform tr p = mkPATransformed p tr

let start x = of_metapath (start x)

let append ?style x y = of_metapath (append ?style (of_path x) (of_path y))

type t = Types.path

type metapath = Types.metapath

let knotp ?(l = defaultdir) ?(r = defaultdir) p = Types.mkKnot l p r

let knot ?l ?r ?scale p = knotp ?l (S.p ?scale p) ?r

let knotn ?l ?r p = knotp ?l (S.pt p) ?r

let knotlist = List.map (fun (x, y, z) -> Types.mkKnot x y z)

let cycle_tmp ?(dir = defaultdir) ?(style = defaultjoint) p =
  mkPACycle dir style p

let cycle = cycle_tmp

let concat ?style x y = of_metapath (concat ?style (of_path x) y)

(* construct a path with a given style from a knot list *)
let pathk ?style ?cycle l =
  let p = MetaPath.pathk ?style l in
  match cycle with
  | None -> of_metapath p
  | Some style -> metacycle defaultdir style p

let pathp ?style ?cycle l = pathk ?style ?cycle (List.map knotp l)

let pathn ?style ?cycle l = pathp ?style ?cycle (List.map Point.pt l)

let path ?style ?cycle ?scale l =
  let sc = S.ptlist ?scale in
  pathp ?style ?cycle (sc l)

(* construct a path with knot list and joint list *)
let jointpathk lp lj = of_metapath (MetaPath.jointpathk lp lj)

let jointpathp lp lj = jointpathk (List.map knotp lp) lj

let jointpathn lp lj = jointpathk (List.map knotn lp) lj

let jointpath ?scale lp lj = jointpathk (List.map (knot ?scale) lp) lj

let scale f p = mkPATransformed p [ Transform.scaled f ]

let rotate f p = mkPATransformed p [ Transform.rotated f ]

let shift pt p = mkPATransformed p [ Transform.shifted pt ]

let yscale n p = mkPATransformed p [ Transform.yscaled n ]

let xscale n p = mkPATransformed p [ Transform.xscaled n ]

let point (f : float) p =
  let p = Compute.path p in
  Spline_lib.abscissa_to_point p f

let direction (f : float) p =
  let p = Compute.path p in
  Spline_lib.direction_of_abscissa p f

let pointn = point

let directionn = direction

let strip n p =
  let p0 = point 0. p in
  let p1 = point (length p) p in
  let c = scale n fullcircle in
  cut_after (shift p1 c) (cut_before (shift p0 c) p)

(* directed paths *)

type orientation =
  | Up
  | Down
  | Left
  | Right
  | Upn of Num.t
  | Downn of Num.t
  | Leftn of Num.t
  | Rightn of Num.t

let divise_dir l =
  let rec fct left_down right_up listn = function
    | [] -> (left_down, right_up, listn)
    | ((Leftn _ | Rightn _ | Downn _ | Upn _) as x) :: res ->
        fct left_down right_up (x :: listn) res
    | ((Left | Down) as x) :: res -> fct (x :: left_down) right_up listn res
    | ((Right | Up) as x) :: res -> fct left_down (x :: right_up) listn res
  in
  fct [] [] [] l

open Num
open Num.Infix
open Point

let dist_horizontal dirlist abs distance =
  let left, right, listn = divise_dir dirlist in
  let diff = List.length right - List.length left in
  let distance = gmean distance zero in
  let d =
    List.fold_left
      (fun a x ->
        match x with
        | Leftn n -> a -/ n
        | Rightn n -> a +/ n
        | _ -> failwith "impossible")
      distance listn
  in
  let dist, _ =
    if diff = 0 then (bp 10., false) else (gmean (d /./ float diff) zero, true)
  in

  let rec fct acc abs = function
    | [] -> List.rev acc
    | Left :: res ->
        let abs = abs -/ dist in
        fct (abs :: acc) abs res
    | Leftn n :: res ->
        let abs = abs -/ n in
        fct (abs :: acc) abs res
    | Right :: res ->
        let abs = abs +/ dist in
        fct (abs :: acc) abs res
    | Rightn n :: res ->
        let abs = abs +/ n in
        fct (abs :: acc) abs res
    | _ -> failwith "impossible"
  in
  fct [] abs dirlist

let dist_vertical dirlist ordo distance =
  let down, up, listn = divise_dir dirlist in
  let diff = List.length up - List.length down in
  let d =
    List.fold_left
      (fun a x ->
        match x with
        | Downn n -> a -/ n
        | Upn n -> a +/ n
        | _ -> failwith "impossible")
      distance listn
  in

  let dist, _ =
    if diff = 0 then (bp 10., false) else (gmean (d /./ float diff) zero, true)
  in
  let rec fct acc ordo = function
    | [] -> List.rev acc
    | Down :: res ->
        let ordo = ordo -/ dist in
        fct (ordo :: acc) ordo res
    | Downn n :: res ->
        let ordo = ordo -/ n in
        fct (ordo :: acc) ordo res
    | Up :: res ->
        let ordo = ordo +/ dist in
        fct (ordo :: acc) ordo res
    | Upn n :: res ->
        let ordo = ordo +/ n in
        fct (ordo :: acc) ordo res
    | _ -> failwith "impossible"
  in
  fct [] ordo dirlist

let smart_path ?style dirlist p1 p2 =
  let width = xpart p2 -/ xpart p1 in
  let height = ypart p2 -/ ypart p1 in
  let dirhorizontal, dirvertical =
    List.partition
      (fun x ->
        match x with Left | Right | Leftn _ | Rightn _ -> true | _ -> false)
      dirlist
  in
  let lesdisth = dist_horizontal dirhorizontal (xpart p1) width in
  let lesdistv = dist_vertical dirvertical (ypart p1) height in

  let rec fct pc acc dirl dv dh =
    match (dirl, dv, dh) with
    | (Up | Upn _ | Down | Downn _) :: dres, dv :: dvres, dhlist ->
        let ps = pt (xpart pc, dv) in
        fct ps (ps :: acc) dres dvres dhlist
    | (Left | Leftn _ | Right | Rightn _) :: dres, dvlist, dh :: dhres ->
        let ps = pt (dh, ypart pc) in
        fct ps (ps :: acc) dres dvlist dhres
    | [], _, _ -> List.rev (p2 :: acc)
    | _ -> assert false
  in
  let points = fct p1 [ p1 ] dirlist lesdistv lesdisth in
  pathp ?style points

let draw ?brush ?color ?pen ?dashed t =
  (* We don't use a default to avoid the output of
     ... withcolor (0.00red+0.00green+0.00blue) withpen ....
     for each command in the output file *)
  mkCommand (mkCDraw t (mkBrushOpt brush color pen dashed))

let fill ?color t = mkCommand (mkCFill t color)
