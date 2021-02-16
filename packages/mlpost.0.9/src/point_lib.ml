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

type t = point = { x : float; y : float }

let zero = { x = 0.; y = 0. }

let add a b = { x = a.x +. b.x; y = a.y +. b.y }

let sub a b = { x = a.x -. b.x; y = a.y -. b.y }

let opp a = { x = -.a.x; y = -.a.y }

let mult c a = { x = a.x *. c; y = a.y *. c }

let div a c = { x = a.x /. c; y = a.y /. c }

let transform m p =
  {
    x = (m.xx *. p.x) +. (m.xy *. p.y) +. m.x0;
    y = (m.yx *. p.x) +. (m.yy *. p.y) +. m.y0;
  }

(* copied here from matrix.ml to avoid dependency *)
let init_rotate a =
  let s = sin a in
  let c = cos a in
  { xx = c; yx = s; xy = -.s; yy = c; x0 = 0.; y0 = 0. }

let rotated f = transform (init_rotate f)

let swapmx { x; y } = { x = y; y = -.x }

let swapmy { x; y } = { x = -.y; y = x }

module Infix = struct
  let ( +/ ) = add

  let ( -/ ) = sub

  let ( */ ) = mult

  let ( // ) = div
end

open Infix

let segment f p1 p2 = ((1. -. f) */ p1) +/ (f */ p2)

let middle = segment 0.5

let print fmt x = Format.fprintf fmt "(%f,%f)" x.x x.y

let norm2 p : float = (p.x *. p.x) +. (p.y *. p.y)

let norm p = sqrt (norm2 p)

let dist2 a b = norm2 (a -/ b)

let dist a b = sqrt (dist2 a b)

let list_min_max f =
  List.fold_left
    (fun ({ x = x_min; y = y_min }, { x = x_max; y = y_max }) s ->
      let { x = sx_min; y = sy_min }, { x = sx_max; y = sy_max } = f s in
      ( { x = min x_min sx_min; y = min y_min sy_min },
        { x = max x_max sx_max; y = max y_max sy_max } ))
    ({ x = infinity; y = infinity }, { x = neg_infinity; y = neg_infinity })

let list_min_max_float f p =
  List.fold_left
    (fun (x_min, y_min, x_max, y_max) s ->
      let sx_min, sy_min, sx_max, sy_max = f s in
      (min x_min sx_min, min y_min sy_min, max x_max sx_max, max y_max sy_max))
    (infinity, infinity, neg_infinity, neg_infinity)
    p

let sign f = if f = 0. then 0. else if f < 0. then -1. else 1.

let sign { x; y } = { x = sign x; y = sign y }

let norm_infinity default f =
  if f = infinity || f = neg_infinity then default else f

let norm_infinity { x = xdef; y = ydef } { x; y } =
  { x = norm_infinity xdef x; y = norm_infinity ydef y }
