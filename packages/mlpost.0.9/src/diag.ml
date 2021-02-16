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

open Helpers

module Node = struct
  type t = {
    box_style : (Box.t -> Box.t) option;
    id : int;
    fill : Color.t option;
    boxed : bool option;
    x : float;
    y : float;
    s : Box.t;
  }

  let create =
    let c = ref min_int in
    fun style fill boxed x y s ->
      incr c;
      { box_style = style; id = !c; fill; boxed; x; y; s }

  let hash n = n.id

  let equal n1 n2 = n1.id == n2.id
end

module Hnode = Hashtbl.Make (Node)
open Node

type node = Node.t

let node ?style ?fill ?boxed x y s = Node.create style fill boxed x y s

type dir = Up | Down | Left | Right | Angle of float

type arrow = {
  src : node;
  dst : node;
  lab : string;
  line_width : Num.t option;
  boxed : bool;
  line_color : Color.t option;
  fill_color : Color.t option;
  head : bool;
  dashed : Types.dash option;
  pos : Command.position option;
  outd : dir option;
  ind : dir option;
}

type t = {
  nodes : node list;
  boxes : Box.t Hnode.t;
  mutable arrows : arrow list;
}

let create l = { nodes = l; boxes = Hnode.create 17; arrows = [] }

let arrow d ?(lab = "") ?line_width ?(boxed = true) ?line_color ?fill_color ?pos
    ?(head = true) ?dashed ?outd ?ind n1 n2 =
  d.arrows <-
    {
      src = n1;
      dst = n2;
      lab;
      line_width;
      boxed;
      line_color;
      fill_color;
      head;
      dashed;
      pos;
      outd;
      ind;
    }
    :: d.arrows

let outdir = function
  | Up -> Path.vec Point.up
  | Down -> Path.vec Point.down
  | Left -> Path.vec Point.left
  | Right -> Path.vec Point.right
  | Angle f -> Path.vec (Point.dir f)

let indir = function
  | Up -> Path.vec Point.down
  | Down -> Path.vec Point.up
  | Left -> Path.vec Point.right
  | Right -> Path.vec Point.left
  | Angle f -> Path.vec (Point.dir f)

let outdir = function None -> None | Some x -> Some (outdir x)

let indir = function None -> None | Some x -> Some (indir x)

type node_style = Box.t -> Box.t

let make_box ?fill ?boxed ~style ~scale d n =
  let p = Point.pt (scale n.x, scale n.y) in
  let pic = n.s in
  let b = match n.box_style with None -> style pic | Some f -> f pic in
  let b = Box.center p b in
  let b = match fill with None -> b | Some f -> Box.set_fill f b in
  let b =
    match boxed with
    | None -> b
    | Some true -> Box.set_stroke Color.black b
    | Some false -> Box.clear_stroke b
  in
  Hnode.add d.boxes n b;
  b

let box_of d = Hnode.find d.boxes

let draw_arrow ?stroke ?pen ?dashed:_ d a =
  let src = box_of d a.src in
  let dst = box_of d a.dst in
  match a.line_width with
  | None ->
      let ba, bla =
        if a.head then (box_arrow, box_label_arrow)
        else (box_line, box_label_line)
      in
      let color = match a.line_color with None -> stroke | Some _ as c -> c in
      if a.lab = "" then
        ba ?color ?pen ?dashed:a.dashed ?outd:(outdir a.outd) ?ind:(indir a.ind)
          src dst
      else
        bla ?color ?pen ?dashed:a.dashed ?outd:(outdir a.outd)
          ?ind:(indir a.ind) ?pos:a.pos (Picture.tex a.lab) src dst
  | Some width ->
      let path = Box.cpath ?outd:(outdir a.outd) ?ind:(indir a.ind) src dst in
      let src = Path.point 0. path in
      let dst = Path.point 1. path in
      Arrow.draw_thick ~boxed:a.boxed ?line_color:a.line_color
        ?fill_color:a.fill_color ?outd:(outdir a.outd) ?ind:(indir a.ind) ~width
        src dst

let fortybp x = Num.bp (40. *. x)

let defaultbox s = Box.round_rect ~dx:Num.two ~dy:Num.two s

let draw ?(scale = fortybp) ?(style = defaultbox) ?boxed ?fill ?stroke ?pen d =
  let l =
    List.map
      (fun n ->
        let fill = if n.fill <> None then n.fill else fill in
        let boxed = if n.Node.boxed <> None then n.Node.boxed else boxed in
        Box.draw (make_box ?fill ?boxed ~style ~scale d n))
      d.nodes
  in
  Command.seq (l @ List.map (draw_arrow ?stroke ?pen d) d.arrows)
