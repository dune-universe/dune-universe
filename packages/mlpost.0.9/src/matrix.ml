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

open Ctypes

type point = Ctypes.point

type t = matrix = {
  mutable xx : float;
  mutable yx : float;
  mutable xy : float;
  mutable yy : float;
  mutable x0 : float;
  mutable y0 : float;
}

(* specialized negation here to avoid dependency *)
let neg_point p = { x = -.p.x; y = -.p.y }

let linear xx xy yx yy = { xx; xy; yx; yy; x0 = 0.; y0 = 0. }

(*
let scale_mult m f =
  { xx = m.xx *. f;
    xy = m.xy *. f;
    yx = m.yx *. f;
    yy = m.yy *. f;
    x0 = m.x0 *. f;
    y0 = m.y0 *. f;
  }
*)

let init_translate x y = { xx = 1.; yx = 0.; xy = 0.; yy = 1.; x0 = x; y0 = y }

let init_scale x y = { xx = x; yx = 0.; xy = 0.; yy = y; x0 = 0.; y0 = 0. }

let init_identity = { xx = 1.; yx = 0.; xy = 0.; yy = 1.; x0 = 0.; y0 = 0. }

let init_rotate a =
  let s = sin a in
  let c = cos a in
  { xx = c; yx = s; xy = -.s; yy = c; x0 = 0.; y0 = 0. }

let multiply a b =
  {
    xx = (a.xx *. b.xx) +. (a.yx *. b.xy);
    yx = (a.xx *. b.yx) +. (a.yx *. b.yy);
    xy = (a.xy *. b.xx) +. (a.yy *. b.xy);
    yy = (a.xy *. b.yx) +. (a.yy *. b.yy);
    x0 = (a.x0 *. b.xx) +. (a.y0 *. b.xy) +. b.x0;
    y0 = (a.x0 *. b.yx) +. (a.y0 *. b.yy) +. b.y0;
  }

let translate m tx ty = multiply (init_translate tx ty) m

let translation p = init_translate p.x p.y

let xy_translation x y = init_translate x y

let rotation = init_rotate

let scale f = init_scale f f

let xscaled f = init_scale f 1.

let yscaled f = init_scale 1. f

let slanted f = linear 1. f 0. 1.

let zscaled p = linear p.x (0. -. p.y) p.y p.x

let reflect _p1 _p2 = (*TODO *) assert false

let rotate m f = multiply (init_rotate f) m

let rotate_around p f = translate (rotate (translation (neg_point p)) f) p.x p.y

let identity = init_identity

let remove_translation t = { t with x0 = 0.; y0 = 0. }

let print fmt m =
  Format.fprintf fmt "[|[|%f;%f|];[|%f;%f|];[|%f;%f|]|]" m.xx m.xy m.yx m.yy
    m.x0 m.y0
