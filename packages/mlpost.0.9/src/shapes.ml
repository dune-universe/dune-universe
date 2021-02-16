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

open Num
open Point
open Path
open Num.Infix
open Types

let pi = Num.pi

let kappa = 4. *. (sqrt 2. -. 1.) /. 3.

let mkappa = 1. -. (4. *. (sqrt 2. -. 1.) /. 3.)

type t = Path.t

(** Rectangles *)

let round_rect width height rx ry =
  let hw, hh = (width /./ 2., height /./ 2.) in
  let rx = maxn zero (minn rx hw) in
  let ry = maxn zero (minn ry hh) in
  (*     let ul, ur, br, bl =  *)
  (*       pt (-.hw, hh), pt (hw, hh),  *)
  (*       pt (hw, -.hh), pt (-.hw, -.hh) in *)
  let ul1, ul2 =
    (pt (neg hw, hh -/ (mkappa */ ry)), pt ((mkappa */ rx) -/ hw, hh))
  in
  let ur1, ur2 =
    (pt (hw -/ (mkappa */ rx), hh), pt (hw, hh -/ (mkappa */ ry)))
  in
  let br1, br2 =
    (pt (hw, (mkappa */ ry) -/ hh), pt (hw -/ (mkappa */ rx), neg hh))
  in
  let bl1, bl2 =
    (pt ((mkappa */ rx) -/ hw, neg hh), pt (neg hw, (mkappa */ ry) -/ hh))
  in
  let knots =
    knotlist
      [
        (noDir, pt (rx -/ hw, hh), mkVec right);
        (noDir, pt (hw -/ rx, hh), mkVec right);
        (noDir, pt (hw, hh -/ ry), mkVec down);
        (noDir, pt (hw, ry -/ hh), mkVec down);
        (noDir, pt (hw -/ rx, neg hh), mkVec left);
        (noDir, pt (rx -/ hw, neg hh), mkVec left);
        (noDir, pt (neg hw, ry -/ hh), mkVec up);
        (noDir, pt (neg hw, hh -/ ry), mkVec up);
      ]
  in
  let joints =
    [
      jLine;
      mkJControls ur1 ur2;
      jLine;
      mkJControls br1 br2;
      jLine;
      mkJControls bl1 bl2;
      jLine;
    ]
  in
  cycle ~dir:(mkVec right) ~style:(mkJControls ul1 ul2)
    (jointpathk knots joints)

(** Ellipses and Arcs *)

let ellipse rx ry =
  let m theta = pt (rx */ cos theta, ry */ sin theta) in
  let knots =
    knotlist
      [
        (noDir, m 0., mkVec up);
        (noDir, m (pi /. 2.), mkVec left);
        (noDir, m pi, mkVec down);
        (noDir, m (-.pi /. 2.), mkVec right);
      ]
  in
  let r1, r2 = (pt (rx, neg (ry */ kappa)), pt (rx, ry */ kappa)) in
  let t1, t2 = (pt (rx */ kappa, ry), pt (neg (rx */ kappa), ry)) in
  let l1, l2 = (pt (neg rx, ry */ kappa), pt (neg rx, neg (ry */ kappa))) in
  let b1, b2 = (pt (neg (rx */ kappa), neg ry), pt (rx */ kappa, neg ry)) in
  let joints = [ mkJControls r2 t1; mkJControls t2 l1; mkJControls l2 b1 ] in
  cycle ~dir:(mkVec up) ~style:(mkJControls b2 r1) (jointpathk knots joints)

let round_box width height =
  let w = width /./ 2. and h = height /./ 2. in
  let mw = neg w and mh = neg h in
  let dx = h /./ 5. and dy = h /./ 5. in
  let style = jCurveNoInflex in
  Path.pathn ~cycle:style ~style
    [ (mw -/ dx, zero); (zero, mh -/ dy); (w +/ dx, zero); (zero, h +/ dy) ]

(*
let arc_ellipse_path ?(close=false) rx ry theta1 theta2 =
  let curvabs theta =  (2. *. theta) /. pi in
  let path =
    subpath (curvabs theta1) (curvabs theta2) (full_ellipse_path rx ry) in
    if close then
      cycle ~style:JLine (concat ~style:JLine path (NoDir,origin,NoDir))
    else path
*)

let rectangle width height =
  let w = width /./ 2. in
  let h = height /./ 2. in
  let mw = neg w in
  let mh = neg h in
  Path.pathn ~cycle:jLine ~style:jLine [ (w, mh); (w, h); (mw, h); (mw, mh) ]

let patatoid width height =
  let wmin, wmax = (-0.5 *./ width, 0.5 *./ width) in
  let hmin, hmax = (-0.5 *./ height, 0.5 *./ height) in
  let ll = pt (wmin, hmin) in
  let lr = pt (wmax, hmin) in
  let ur = pt (wmax, hmax) in
  let ul = pt (wmin, hmax) in
  let a = segment (Random.float 1.) ll lr in
  let b = segment (Random.float 1.) lr ur in
  let c = segment (Random.float 1.) ur ul in
  let d = segment (Random.float 1.) ul ll in
  pathp ~cycle:jCurve [ a; b; c; d ]

let patatoid2 width height =
  let wmin, wmax = (-0.5 *./ width, 0.5 *./ width) in
  let hmin, hmax = (-0.5 *./ height, 0.5 *./ height) in
  let ll = pt (wmin, hmin) in
  let lr = pt (wmax, hmin) in
  let ur = pt (wmax, hmax) in
  let ul = pt (wmin, hmax) in
  let f cl cr x y p =
    let d = pt (bp x, bp y) in
    let r = Point.rotate (Random.float 60. -. 30.) d in
    let l = Point.sub Point.origin r in
    let r = Point.scale ((Random.float 1. +. 0.5) *./ cl) r in
    let l = Point.scale ((Random.float 1. +. 0.5) *./ cr) l in
    (Point.shift p l, Point.shift p r)
  in
  let c = 0.25 in
  let ch = c *./ width in
  let cv = c *./ height in
  let l1, r1 = f ch cv 1. 1. ul in
  let l2, r2 = f cv ch 1. (-1.) ur in
  let l3, r3 = f ch cv (-1.) (-1.) lr in
  let l4, r4 = f cv ch (-1.) 1. ll in
  let path =
    jointpathp [ ul; ur; lr; ll ]
      [ jControls r1 l2; jControls r2 l3; jControls r3 l4 ]
  in
  cycle ~style:(jControls r4 l1) path

let circle d = Path.scale d Path.fullcircle
