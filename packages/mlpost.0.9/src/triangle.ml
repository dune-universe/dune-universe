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
open Color

type t = Box.t

let draw = Box.draw

(** width defaults to depth *)
let create ?brush ?(stroke = Some black) ?pen ?dash ?fill ?left ?right ?width
    ~depth () =
  let width = match width with None -> depth | Some w -> w in
  let xl, xr =
    match (left, right) with
    | None, None -> (-0.5, 0.5)
    | Some l, None -> (-.l, 1. -. l)
    | None, Some r -> (r -. 1., r)
    | Some l, Some r ->
        let s = l +. r in
        (-.l /. s, r /. s)
  in
  let xl = xl *. width in
  let xr = xr *. width in
  let xmin = min 0. xl in
  let width = max 0. xr -. xmin in
  let dx = (width /. 2.) +. xmin in
  let height = depth in
  let p0 = Point.pt (-.dx, height /. 2.) in
  let p1 = Point.pt (xl -. dx, -.height /. 2.) in
  let p2 = Point.pt (xr -. dx, -.height /. 2.) in
  let p = Path.pathp ~style:Path.jLine ~cycle:Path.jLine [ p0; p1; p2 ] in
  let f _ _ = (width, depth, p) in
  let style = Box.Custom f in
  let b = Box.empty ?brush ~stroke ?pen ?dash ?fill ~style () in
  let b = Box.setp "root" p0 b in
  let b = Box.setp "bottom_left" p1 b in
  let b = Box.setp "bottom_right" p2 b in
  b

let root = Box.getp "root"

let bottom_left = Box.getp "bottom_left"

let bottom_right = Box.getp "bottom_right"

let root_label ?(pos = `Top) label t =
  Box.add_post_draw (fun b -> Command.label ~pos label (root b)) t

let tex_root_label ?pos s t = root_label ?pos (Picture.tex s) t

(** point (x,depth) in a term *)
let x_depth ?(x = 0.5) ?(depth = 1.) t2 =
  let bl2 = bottom_left t2 in
  let br2 = bottom_right t2 in
  let x = Point.xpart bl2 +. (x *. (Point.xpart br2 -. Point.xpart bl2)) in
  let y = Point.ypart bl2 in
  let p = Point.pt (x, y) in
  let r2 = root t2 in
  Point.add r2 (Point.mult depth (Point.sub p r2))

let label ?x ?(depth = 0.7) ?pos label t =
  Box.add_post_draw
    (fun t ->
      let p = x_depth ?x ~depth t in
      Command.label ?pos label p)
    t

let tex_label ?x ?depth ?pos s t = label ?x ?depth ?pos (Picture.tex s) t

(** move root of t1 to point (depth,x) in t2 *)
let anchor ?x ?depth t2 t1 =
  let p = x_depth ?x ?depth t2 in
  Box.shift (Point.sub p (root t1)) t1

let pose ?x ?depth t2 t1 =
  let p = x_depth ?x ?depth t2 in
  Box.shift (Point.sub p (Box.south t1)) t1

(* FIXME: rather middle of bl and br? *)

let pose_left ?x ?depth t2 t1 =
  let p = x_depth ?x ?depth t2 in
  Box.shift (Point.sub p (bottom_left t1)) t1

let pose_right ?x ?depth t2 t1 =
  let p = x_depth ?x ?depth t2 in
  Box.shift (Point.sub p (bottom_right t1)) t1

let pic ?brush ?stroke ?pen ?dash ?fill ?(dx = bp 1.) ?(dy = bp 1.) pic =
  let width = 2. *. (Picture.width pic +. (2. *. dx)) in
  let depth = 2. *. (Picture.height pic +. (2. *. dy)) in
  let t = create ?brush ?stroke ?pen ?dash ?fill ~width ~depth () in
  label ~depth:0.75 pic t

let tex ?brush ?stroke ?pen ?dash ?fill ?dx ?dy s =
  pic ?brush ?stroke ?pen ?dash ?fill ?dx ?dy (Picture.tex s)
