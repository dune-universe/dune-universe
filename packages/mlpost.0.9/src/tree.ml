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

open Command
open Path
open Num
open Num.Infix

type arrow_style = Directed | Undirected

type edge_style = Straight | Curve | Square | HalfSquare

type 'a tree = Node of 'a * 'a tree list

type extend = (Num.t * Num.t) list

type 'a positionedtree = ('a * Num.t * Num.t) tree

let movetree (Node ((label, x, y), subtrees)) dx =
  Node ((label, x +/ dx, y), subtrees)

let moveextend (e : extend) (x : Num.t) : extend =
  List.map (fun (p, q) -> (p +/ x, q +/ x)) e

let rec merge a1 a2 =
  match (a1, a2) with
  | [], rt -> rt
  | lt, [] -> lt
  | (l, _) :: t1, (_, r) :: t2 -> (l, r) :: merge t1 t2

let mergelist l = List.fold_right merge l []

let rec fit cs (a1 : extend) (a2 : extend) : Num.t =
  match (a1, a2) with
  | (_, r) :: t1, (l, _) :: t2 -> maxn (fit cs t1 t2) (r -/ l +/ cs)
  | _ -> bp 0.

let fitlistl cs (l : extend list) : Num.t list =
  let rec fitlistaux acc li =
    match li with
    | [] -> []
    | e :: res ->
        let x = fit cs acc e in
        x :: fitlistaux (merge acc (moveextend e x)) res
  in
  fitlistaux [] l

let fitlistr cs (l : extend list) : Num.t list =
  let rec fitlistaux acc li =
    match li with
    | [] -> []
    | e :: res ->
        let x = neg (fit cs e acc) in
        x :: fitlistaux (merge (moveextend e x) acc) res
  in
  List.rev (fitlistaux [] (List.rev l))

let mean x y = (x +/ y) // bp 2.

let fitlist cs (l : extend list) : Num.t list =
  List.map2 mean (fitlistl cs l) (fitlistr cs l)

let bfs t =
  let rec bfs_aux lesmax m current next =
    match current with
    | [] ->
        if next = [] then List.rev (m :: lesmax)
        else bfs_aux (m :: lesmax) (bp 0.) next []
    | Node (x, tl) :: cl ->
        let (b, _), _, _ = x in
        let m = maxn m (Box.height b) in
        bfs_aux lesmax m cl (tl @ next)
  in
  bfs_aux [] (bp 0.) [ t ] []

let rec dfs lesmax t =
  match (t, lesmax) with
  | Node ((b, x, _), tl), m :: mres -> Node ((b, x, m), List.map (dfs mres) tl)
  | _, [] -> failwith "impossible"

type node_info = {
  ls : Num.t;
  cs : Num.t;
  arrow_style : arrow_style;
  edge_style : edge_style;
  stroke : Color.t option;
  pen : Pen.t option;
  sep : Num.t option;
  lab : (Command.position * Picture.t) list option;
}

let dummy_info =
  {
    ls = zero;
    cs = zero;
    arrow_style = Directed;
    edge_style = Straight;
    stroke = None;
    pen = None;
    sep = None;
    lab = None;
  }

let design tree =
  let rec designaux (Node (((b, ni) as label), subtrees)) =
    let trees, extends = List.split (List.map designaux subtrees) in
    let positions = fitlist ni.cs extends in
    let ptrees = List.map2 movetree trees positions in
    let pextends = List.map2 moveextend extends positions in
    let w = divf (Box.width b) 2. in
    let resultextend = (neg w, w) :: mergelist pextends in
    let resulttree = Node ((label, bp 0., bp 0.), ptrees) in
    (resulttree, resultextend)
  in
  let thetree = fst (designaux tree) in
  let maxlistheight = bfs thetree in
  dfs maxlistheight thetree

(* drawing *)

let strip ?sep p = match sep with None -> p | Some n -> Path.strip n p

let cpath ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  let p = pathk ?style [ knotp ?r (Box.ctr a); knotp ?l (Box.north b) ] in
  strip ?sep (cut_before (Box.bpath a) p)

let box_arrow ?color ?brush ?pen ?dashed ?style ?outd ?ind ?sep a b =
  Arrow.simple ?color ?brush ?pen ?dashed (cpath ?style ?outd ?ind ?sep a b)

let box_line ?color ?brush ?pen ?dashed ?style ?outd ?ind ?sep a b =
  draw ?color ?brush ?pen ?dashed (cpath ?style ?outd ?ind ?sep a b)

let arc astyle estyle ?stroke ?brush ?pen ?sep ?lab b1 b2 =
  let x1, y1 =
    let p = Box.ctr b1 in
    (Point.xpart p, Point.ypart p)
  and x2, y2 =
    let p = Box.north b2 in
    (Point.xpart p, Point.ypart p)
  in
  let boxdraw, linedraw =
    match astyle with
    | Directed ->
        ( box_arrow ?color:stroke ?brush ?pen ?sep,
          Arrow.simple ?color:stroke ?brush ?pen )
    | Undirected ->
        (box_line ?color:stroke ?brush ?pen ?sep, draw ?color:stroke ?brush ?pen)
  in
  let drawlab b1 b2 = function
    | None -> nop
    | Some (pos, lab) ->
        let p = Box.cpath b1 b2 in
        label ~pos lab (Path.point 0.5 p)
  in
  match estyle with
  | Straight -> boxdraw ~style:jLine b1 b2 ++ drawlab b1 b2 lab
  | Curve ->
      let p1, p2 = (Box.ctr b1, Box.ctr b2) in
      let corner = Point.pt (x2 -/ ((x2 -/ x1) /./ 4.), (y1 +/ y2) /./ 2.) in
      let p =
        pathk ~style:jCurve
          (knotlist
             [
               (noDir, p1, vec (Point.sub corner p1));
               (noDir, corner, noDir);
               (vec (Point.sub p2 corner), p2, noDir);
             ])
      in
      let parrow = cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) in
      linedraw (strip ?sep parrow)
  | Square ->
      let corner = Point.pt (x2, y1) in
      let p = pathp ~style:jLine [ Box.ctr b1; corner; Box.ctr b2 ] in
      let parrow = cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) in
      linedraw (strip ?sep parrow)
  | HalfSquare ->
      let m = (y1 +/ y2) /./ 2. in
      let corner1, corner2 = (Point.pt (x1, m), Point.pt (x2, m)) in
      let p = pathp ~style:jLine [ Box.ctr b1; corner1; corner2; Box.ctr b2 ] in
      let parrow = cut_after (Box.bpath b2) (cut_before (Box.bpath b1) p) in
      linedraw (strip ?sep parrow)

let is_leaf x = Array.length (Box.elts x) = 1

let root x =
  (* if this access is invalid, the box has not been created using
   * [leaf], [node] or [bin] *)
  Box.nth 0 x

let children x = if is_leaf x then [] else Box.elts_list (Box.nth 1 x)

type t = (Box.t * node_info) tree

let leaf b = Node ((b, dummy_info), [])

let node ?(ls = bp 20.) ?(cs = bp 3.) ?(arrow_style = Directed)
    ?(edge_style = Straight) ?stroke ?brush:_ ?pen ?sep b l =
  Node
    ((b, { ls; cs; arrow_style; edge_style; stroke; pen; sep; lab = None }), l)

let nodel ?(ls = bp 20.) ?(cs = bp 3.) ?(arrow_style = Directed)
    ?(edge_style = Straight) ?stroke ?brush:_ ?pen ?sep b l =
  Node
    ( ( b,
        {
          ls;
          cs;
          arrow_style;
          edge_style;
          stroke;
          pen;
          sep;
          lab = Some (List.map snd l);
        } ),
      List.map fst l )

let bin ?ls ?cs ?arrow_style ?edge_style ?stroke ?brush ?pen ?sep s x y =
  node ?ls ?cs ?arrow_style ?edge_style ?stroke ?brush ?pen ?sep s [ x; y ]

let to_box t : Box.t =
  let rec draw x y (Node (((b, ni), dx, dy), tl)) =
    let { stroke; pen; sep; _ } = ni in
    let x' = addn x dx in
    let y' = subn y (divf (Box.height b) 2.) in
    let y2 = subn y (maxn dy ni.ls) in
    let b =
      Box.group
        [
          Box.center (Point.pt (x', y')) b; Box.group (List.map (draw x' y2) tl);
        ]
    in
    let draw_arcs tree =
      match ni.lab with
      | None ->
          Command.iterl
            (fun child ->
              arc
                ~brush:(Types.mkBrush stroke pen None)
                ?sep ni.arrow_style ni.edge_style (root tree) (root child))
            (children tree)
      | Some lab ->
          Command.iterl
            (fun (child, lab) ->
              arc
                ~brush:(Types.mkBrush stroke pen None)
                ?sep ~lab ni.arrow_style ni.edge_style (root tree) (root child))
            (List.combine (children tree) lab)
    in
    Box.set_post_draw draw_arcs b
  in
  draw zero zero (design t)

let draw ?debug t = Box.draw ?debug (to_box t)

module Simple = struct
  type t = Box.t

  let leaf s = Box.group [ s ]

  let node ?(ls = Num.bp 12.) ?(cs = Num.bp 5.) ?(arrow_style = Directed)
      ?(edge_style = Straight) ?stroke ?brush ?pen ?sep ?(valign = `Center)
      ?(halign = `North) s l =
    let l = Box.hbox ~padding:cs ~pos:halign l in
    let tree = Box.vbox ~padding:ls ~pos:valign [ s; l ] in
    Box.set_post_draw
      (fun tree ->
        Command.iterl
          (fun child ->
            arc ?stroke ?brush ?pen ?sep arrow_style edge_style (root tree)
              (root child))
          (children tree))
      tree

  let bin ?ls ?cs ?arrow_style ?edge_style ?stroke ?brush ?pen ?sep s x y =
    node ?ls ?cs ?arrow_style ?edge_style ?stroke ?brush ?pen ?sep s [ x; y ]

  let to_box b = b

  let draw = Box.draw
end
