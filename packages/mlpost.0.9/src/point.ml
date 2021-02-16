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

open Types
open Point_lib

type corner = Types.corner

type t = Ctypes.point

(* angle in degrees *)
let dir f =
  let angle = Num.deg2rad f in
  { x = cos angle; y = sin angle }

let up = { x = 0.; y = 1. }

let down = { x = 0.; y = -1. }

let left = { x = -1.; y = 0. }

let right = { x = 1.; y = 0. }

let xpart p = p.x

let ypart p = p.y

let add = add

let mult = mult

let sub = sub

let shift = add

let scale = mult

let segment = segment

let rotate angle p = rotated (Num.deg2rad angle) p

(* rotate p2 around p1 *)
let rotate_around p1 f p2 = add p1 (rotate f (sub p2 p1))

let xscale f p = { p with x = f *. p.x }

let yscale f p = { p with y = f *. p.y }

let transform (tr : Transform.t) (p : t) : t =
  let t = Compute.transform tr in
  Point_lib.transform t p

let pt (a, b) = { x = a; y = b }

let p ?(scale = Num.bp) (x, y) = { x = scale x; y = scale y }

let length p = Num.gmean (xpart p) (ypart p)

let origin = p (0., 0.)

let ptlist ?scale l = List.map (p ?scale) l

(* construct a point with the right measure *)
let bpp, inp, cmp, mmp, ptp =
  ( p ~scale:Num.bp,
    p ~scale:Num.inch,
    p ~scale:Num.cm,
    p ~scale:Num.mm,
    p ~scale:Num.pt )

(* construct a list of points with the right measure *)
let map_bp, map_in, map_cm, map_mm, map_pt =
  ( ptlist ~scale:Num.bp,
    ptlist ~scale:Num.inch,
    ptlist ~scale:Num.cm,
    ptlist ~scale:Num.mm,
    ptlist ~scale:Num.pt )

let draw ?brush ?color ?pen t =
  (* We don't use a default to avoid the output of
     ... withcolor (0.00red+0.00green+0.00blue) withpen ....
     for each command in the output file *)
  mkCommand
    (mkCDraw
       (mkPAofMPA (mkMPAKnot (mkKnot mkNoDir t mkNoDir)))
       (mkBrushOpt brush color pen None))

let normalize p =
  let l = length p in
  scale (if Num.is_zero l then 0. else 1. /. l) p
