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

open Path

(* Extended arrows. *)

let normalize = Point.normalize

let neg = Point.scale (Num.bp (-1.))

let direction_on_path f p = Path.direction (f *. Path.length p) p

let point_on_path f p = Path.point (f *. Path.length p) p

let subpath_01 f t p =
  let l = Path.length p in
  Path.subpath (f *. l) (t *. l) p

(* Atoms *)

type line = {
  brush : Types.brush;
  from_point : float;
  to_point : float;
  dist : Num.t;
}

type head_description = { hd_command : Command.t; hd_cut : Types.path option }

let make_head ?cut command = { hd_command = command; hd_cut = cut }

type head = Point.t -> Point.t -> head_description

type belt = { clip : bool; rev : bool; point : float; head : head }

type kind = { lines : line list; belts : belt list }

let empty = { lines = []; belts = [] }

let add_line ?brush ?dashed ?color ?pen ?(from_point = 0.) ?(to_point = 1.)
    ?(dist = Num.bp 0.) kind =
  let brush = Types.mkBrushOpt brush color pen dashed in
  { kind with lines = { brush; from_point; to_point; dist } :: kind.lines }

let head_classic_points ?(angle = 60.) ?(size = Num.bp 4.) p dir =
  let dir = Point.scale size dir in
  let dir_a = neg (Point.rotate (angle /. 2.) dir) in
  let dir_b = neg (Point.rotate (-.angle /. 2.) dir) in
  let a = Point.add p dir_a in
  let b = Point.add p dir_b in
  (a, b)

let head_classic ?color ?brush ?pen ?dashed ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style:Path.jLine [ a; p; b ] in
  make_head ~cut:path (Command.draw ?color ?brush ?pen ?dashed path)

let head_triangle ?color ?brush ?pen ?dashed ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style:Path.jLine ~cycle:Path.jLine [ a; p; b ] in
  let cut = Path.pathp ~style:Path.jLine [ a; b ] in
  make_head ~cut (Command.draw ?color ?brush ?pen ?dashed path)

let head_triangle_full ?color ?angle ?size p dir =
  let a, b = head_classic_points ?angle ?size p dir in
  let path = Path.pathp ~style:Path.jLine ~cycle:Path.jLine [ a; p; b ] in
  let cut = Path.pathp ~style:Path.jLine [ a; b ] in
  make_head ~cut (Command.fill ?color path)

let add_belt ?(clip = false) ?(rev = false) ?(point = 0.5)
    ?(head = fun x -> head_classic x) kind =
  { kind with belts = { clip; rev; point; head } :: kind.belts }

let add_head ?head kind = add_belt ~clip:true ~point:1. ?head kind

let add_foot ?head kind = add_belt ~clip:true ~rev:true ~point:0. ?head kind

let parallel_path path dist =
  (* TODO: true parallelism (right now its a bad approximation which only works
     well for straight arrows, or slightly curved arrow with a small dist) *)
  let d = direction_on_path 0.5 path in
  let d = Point.rotate 90. d in
  let d = normalize d in
  let d = Point.mult dist d in
  Path.shift d path

(* Compute the path of a line along an arrow path.
   Return the line (unchanged) and the computed path. *)
let make_arrow_line path line =
  let path =
    if line.from_point <> 0. || line.to_point <> 1. then
      subpath_01 line.from_point line.to_point path
    else path
  in
  let path = parallel_path path line.dist in
  (line, path)

(* Compute the command and the clipping path of a belt along an arrow path.
   Return the belt (unchanged), the command and the clipping path. *)
let make_arrow_belt path belt =
  let p = point_on_path belt.point path in
  let d = normalize (direction_on_path belt.point path) in
  let d = if belt.rev then neg d else d in
  let hd = belt.head p d in
  (belt, hd.hd_command, hd.hd_cut)

(* Clip a line with a belt clipping path if needed. *)
let clip_line_with_belt (line, line_path) (belt, _, clipping_path) =
  let cut =
    match (belt.clip, clipping_path) with
    | true, Some clipping_path ->
        (if belt.rev then Path.cut_before else Path.cut_after) clipping_path
    | false, _ | true, None -> fun x -> x
  in
  (line, cut line_path)

(* Compute the command to draw a line. *)
let draw_line (line, line_path) = Command.draw ~brush:line.brush line_path

let mk_classic ?color () = add_head (add_line ?color empty)

let mk_implies ?color () =
  add_head
    (add_line ?color ~dist:(Num.cm 0.035)
       (add_line ?color ~dist:(Num.cm (-0.035)) empty))

let classic = mk_classic ()

let triangle = add_head ~head:head_triangle (add_line empty)

let triangle_full = add_head ~head:head_triangle_full (add_line empty)

let implies = mk_implies ()

let iff = add_foot implies

let draw ?(kind = triangle_full) ?tex ?(pos = 0.5) ?anchor path =
  let lines, belts = (kind.lines, kind.belts) in
  let lines = List.map (make_arrow_line path) lines in
  let belts = List.map (make_arrow_belt path) belts in
  let lines =
    List.map (fun line -> List.fold_left clip_line_with_belt line belts) lines
  in
  let lines = List.map draw_line lines in
  let belts = List.map (fun (_, x, _) -> x) belts in
  let labels =
    match tex with
    | None -> []
    | Some tex ->
        [ Command.label ?pos:anchor (Picture.tex tex) (point_on_path pos path) ]
  in
  Command.seq (lines @ belts @ labels)

(* Instances *)
type ('a, 'b) arrow_from_to =
  ?kind:kind ->
  ?tex:string ->
  ?pos:float ->
  ?anchor:Command.position ->
  ?style:Path.joint ->
  ?outd:Path.direction ->
  ?ind:Path.direction ->
  ?sep:Num.t ->
  'a ->
  'b ->
  Command.t

let point_to_point ?kind ?tex ?pos ?anchor ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  let path = Path.pathk ?style [ Path.knotp ?r a; Path.knotp ?l b ] in
  let path = match sep with None -> path | Some n -> Path.strip n path in
  draw ?kind ?tex ?pos ?anchor path

let box_to_box ?kind ?tex ?pos ?anchor ?style ?outd ?ind ?sep a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath ?style ?outd ?ind ?sep a b)

let box_to_point ?kind ?tex ?pos ?anchor ?style ?outd ?ind ?sep a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath_left ?style ?outd ?ind ?sep a b)

let point_to_box ?kind ?tex ?pos ?anchor ?style ?outd ?ind ?sep a b =
  draw ?kind ?tex ?pos ?anchor (Box.cpath_right ?style ?outd ?ind ?sep a b)

(*******************************************************************************)
(*                                 To be sorted                                *)
(*******************************************************************************)

let simple_point_point ?style ?outd ?ind ?sep a b =
  let r, l = (outd, ind) in
  Box.strip ?sep (pathk ?style [ knotp ?r a; knotp ?l b ])

(*let normalize p =
  Point.scale (Num.divn (Num.bp 1.) (Point.length p)) p*)

let neg = Point.scale (Num.bp (-1.))

let thick_path ?style ?outd ?ind ?(width = Num.bp 10.)
    ?(head_length = Num.multf 2. width) ?(head_width = head_length) a b =
  let path = simple_point_point ?style ?outd ?ind a b in
  let a_dir = normalize (Path.direction 0. path) in
  let a_normal = Point.rotate 90. a_dir in
  let a1 = Point.add (Point.scale (Num.divf width 2.) a_normal) a in
  let a2 = Point.add (Point.scale (Num.divf width (-2.)) a_normal) a in
  let b_dir = normalize (Path.direction 1. path) in
  let b_normal = Point.rotate 90. b_dir in
  let c = Point.add (Point.scale (Num.neg head_length) b_dir) b in
  let c1 = Point.add (Point.scale (Num.divf width 2.) b_normal) c in
  let c2 = Point.add (Point.scale (Num.divf width (-2.)) b_normal) c in
  let c1' = Point.add (Point.scale (Num.divf head_width 2.) b_normal) c in
  let c2' = Point.add (Point.scale (Num.divf head_width (-2.)) b_normal) c in
  (* let path_ac = simple ?style ?outd ?ind a c in
     let m = Path.point 0.5 path_ac in
     let m_dir = normalize (Path.direction 0.5 path_ac) in
     let m_dir2 = Point.scale (Num.bp 0.) m_dir in
     let m_normal = Point.rotate 90. m_dir in
     let m1 = Point.add (Point.scale (Num.divf width 2.) m_normal) m in
     let m2 = Point.add (Point.scale (Num.divf width (-2.)) m_normal) m in*)
  let path1 =
    pathk ~style:jCurve
      [ knotp ~r:(vec a_dir) a1; (*      knotp m1;*) knotp ~l:(vec b_dir) c1 ]
  in
  let path2 =
    pathk ~style:jCurve
      [
        knotp ~r:(vec (neg b_dir)) c2;
        (*      knotp m2;*)
        knotp ~l:(vec (neg a_dir)) a2;
      ]
  in
  let path_head = pathk ~style:jLine [ knotp c1'; knotp b; knotp c2' ] in
  cycle ~style:jLine
    (append ~style:jLine (append ~style:jLine path1 path_head) path2)

let draw_thick ?style ?(boxed = true) ?line_color ?fill_color ?outd ?ind ?width
    ?head_length ?head_width a b =
  let p = thick_path ?style ?outd ?ind ?width ?head_length ?head_width a b in
  let draw_cmd =
    if boxed then Command.draw ?color:line_color p else Command.nop
  in
  let fill_cmd =
    match fill_color with
    | None -> Command.nop
    | Some c -> Command.fill ~color:c p
  in
  Command.append fill_cmd draw_cmd

let simple ?color ?brush ?pen ?dashed p =
  let kind =
    add_head
      ~head:(head_triangle_full ?color)
      (add_line ?dashed ?color ?brush ?pen empty)
  in
  draw ~kind p
