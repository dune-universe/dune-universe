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

module P = Point_lib
module M = Matrix

let default_labeloffset = 3.5 (* should be 3. but not enough*)

let default_pen = M.scale 0.5

let bbox_offset = { P.x = 2.; P.y = 2. }

let pi = 3.1415926535897932384626433832795029

let pi_div_180 = pi /. 180.0

let deg2rad f = pi_div_180 *. f

open Types
open Hashcons
module S = Spline_lib
module Pi = Picture_lib
module MP = Metapath_lib

let memoize f fname memoize arg =
  try Hashtbl.find memoize arg.tag
  with Not_found ->
    let result =
      try f arg.node
      with exn ->
        if Defaults.get_debug () then
          Format.printf "Compute.%s raises : %s@.@?" fname
            (Printexc.to_string exn);
        raise exn
    in
    Hashtbl.add memoize arg.tag result;
    result

let nop = Picture_lib.empty

let option_compile f = function None -> None | Some obj -> Some (f obj)

let option_def_compile def f = function None -> def | Some obj -> f obj

let middle x y = (x /. 2.) +. (y /. 2.)

let point_of_position ecart ({ P.x = xmin; y = ymin }, { P.x = xmax; y = ymax })
    pos =
  match pos_reduce pos with
  | `North -> { P.x = middle xmin xmax; y = ymax +. ecart }
  | `South -> { P.x = middle xmin xmax; y = ymin -. ecart }
  | `West -> { P.x = xmin -. ecart; y = middle ymin ymax }
  | `East -> { P.x = xmax +. ecart; y = middle ymin ymax }
  | `Northwest -> { P.x = xmin -. ecart; y = ymax +. ecart }
  | `Northeast -> { P.x = xmax +. ecart; y = ymax +. ecart }
  | `Southwest -> { P.x = xmin -. ecart; y = ymin -. ecart }
  | `Southeast -> { P.x = xmax +. ecart; y = ymin -. ecart }
  | `Center -> { P.x = middle xmin xmax; P.y = middle ymin ymax }

let anchor_of_position pos =
  match pos_reduce pos with
  | `North -> `South
  | `South -> `North
  | `West -> `East
  | `East -> `West
  | `Northwest -> `Southeast
  | `Northeast -> `Southwest
  | `Southwest -> `Northeast
  | `Southeast -> `Northwest
  | `Center -> `Center

let metapath_memoize = Hashtbl.create 50

let path_memoize = Hashtbl.create 50

let picture_memoize = Hashtbl.create 50

let command_memoize = Hashtbl.create 50

let clear () =
  Hashtbl.clear metapath_memoize;
  Hashtbl.clear path_memoize;
  Hashtbl.clear picture_memoize;
  Hashtbl.clear command_memoize

let float_to_metapost f =
  (* Compatibility with metapost *)
  if f = infinity then 4095.99998 (* cf mpman *)
  else if f > 4095. then 4095.
  else if abs_float f < 0.0001 then 0.
  else f

let rec num n = n

and point p = p

(*
and point' = function
  | PTPair (f1,f2) ->
      let f1 = num f1 in
      let f2 = num f2 in
      {P.x=f1;y=f2}
  | PTPointOf (f,p) ->
      let p = path p in
      let f = Spline_lib.abscissa_of_metapost p (num f) in
      Spline_lib.abscissa_to_point p f
  | PTDirectionOf (f,p) ->
      let p = path p in
      let f = Spline_lib.abscissa_of_metapost p (num f) in
      Spline_lib.direction_of_abscissa p f
  | PTAdd (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
      P.add p1 p2
  | PTSub (p1,p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
        P.sub p1 p2
  | PTMult (f,p) ->
      let f = num f in
      let p1 = point p in
        P.mult f p1
  | PTRotated (f,p) ->
      let p1 = point p in
      P.rotated (deg2rad f) p1
  | PTPicCorner (pic, corner) ->
      let p = commandpic pic in
      point_of_position 0. (Picture_lib.bounding_box p) (corner :> position)
  | PTTransformed (p,tr) ->
      let p = point p in
      let tr = transform tr in
      P.transform tr p
and point p = memoize point' "point" point_memoize p
*)
and knot k =
  match k.Hashcons.node with
  | { knot_in = d1; knot_p = p; knot_out = d2 } ->
      let d1 = direction d1 in
      let p = point p in
      let d2 = direction d2 in
      let d1, d2 = MP.equalize_dir (d1, d2) in
      (d1, MP.knot p, d2)

and joint dl j dr =
  match j.Hashcons.node with
  | JLine -> MP.line_joint
  | JCurve -> MP.curve_joint dl dr
  | JCurveNoInflex -> MP.curve_no_inflex_joint dl dr
  | JTension (a, b) -> MP.tension_joint dl a b dr
  | JControls (p1, p2) ->
      let p1 = point p1 in
      let p2 = point p2 in
      MP.controls_joint p1 p2

and direction d =
  match d.Hashcons.node with
  | Vec p ->
      let p = point p in
      MP.vec_direction p
  | Curl f -> MP.curl_direction (float_to_metapost f)
  | NoDir -> MP.no_direction

and metapath' = function
  | MPAConcat (pa, j, p) ->
      let pdl, p, pdr = metapath p in
      let dl, pa, dr = knot pa in
      let j = joint pdr j dl in
      (pdl, MP.concat p j pa, dr)
  | MPAAppend (p1, j, p2) ->
      let p1dl, p1, p1dr = metapath p1 in
      let p2dl, p2, p2dr = metapath p2 in
      let j = joint p1dr j p2dl in
      (p1dl, MP.append p1 j p2, p2dr)
  | MPAKnot k ->
      let dl, p, dr = knot k in
      (dl, MP.start p, dr)
  | MPAofPA p -> (MP.no_direction, MP.from_path (path p), MP.no_direction)

and metapath p = memoize metapath' "metapath" metapath_memoize p

and path' = function
  | PAofMPA p ->
      let _, mp, _ = metapath p in
      MP.to_path mp
  | MPACycle (d, j, p) ->
      let d = direction d in
      let _, p, dr = metapath p in
      let j = joint dr j d in
      MP.cycle j p
  | PATransformed (p, tr) ->
      let p = path p in
      let tr = transform tr in
      Spline_lib.transform tr p
  | PACutAfter (p1, p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_after p1 p2
  | PACutBefore (p1, p2) ->
      let p1 = path p1 in
      let p2 = path p2 in
      Spline_lib.cut_before p1 p2
  | PABuildCycle _pl ->
      (*       let npl = List.map path pl in *)
      (* TODO *) assert false
  (*       Spline_lib.buildcycle npl *)
  | PASub (f1, f2, p) ->
      let p = path p in
      Spline_lib.subpath p (num f1) (num f2)
  | PABBox p ->
      let p = commandpic p in
      let pmin, pmax = Picture_lib.bounding_box p in
      let pmin, pmax = (P.sub pmin bbox_offset, P.add pmax bbox_offset) in
      Spline_lib.close
        (Spline_lib.create_lines
           [
             { P.x = pmin.P.x; y = pmin.P.y };
             { P.x = pmin.P.x; y = pmax.P.y };
             { P.x = pmax.P.x; y = pmax.P.y };
             { P.x = pmax.P.x; y = pmin.P.y };
           ])
  | PAUnitSquare -> MP.Approx.unitsquare 1.
  | PAQuarterCircle -> MP.Approx.quartercircle 1.
  | PAHalfCircle -> MP.Approx.halfcirle 1.
  | PAFullCircle -> MP.Approx.fullcircle 1.

and path p =
  (*Format.printf "path : %a@.@?" Print.path p;*)
  memoize path' "path" path_memoize p

and picture' = function
  | PITransformed (p, tr) ->
      let tr = transform tr in
      let pic = commandpic p in
      Picture_lib.transform tr pic
  | PITex s ->
      let tex = Gentex.create s in
      Picture_lib.tex tex
  | PIClip (pic, pth) ->
      let pic = commandpic pic in
      let pth = path pth in
      Picture_lib.clip pic pth

and picture p = memoize picture' "picture" picture_memoize p

and transform t = List.fold_left Matrix.multiply Matrix.identity t

and commandpic p =
  match p.Hashcons.node with
  | Picture p -> picture p
  | Command c -> command c
  | Seq l -> (
      match l with
      | [] -> Picture_lib.empty
      | [ x ] -> commandpic x
      | x :: r ->
          List.fold_left
            (fun acc c -> Picture_lib.on_top acc (commandpic c))
            (commandpic x) r )

and dash d =
  match d.Hashcons.node with
  | DEvenly -> Picture_lib.Dash.line
  | DWithdots -> Picture_lib.Dash.dots
  | DScaled (f, d) ->
      let f = num f in
      let d = dash d in
      Picture_lib.Dash.scale f d
  | DShifted (p, d) ->
      let p = point p in
      let d = dash d in
      Picture_lib.Dash.shifted p.P.x d
  | DPattern l ->
      let l = List.map dash_pattern l in
      Picture_lib.Dash.pattern l

and dash_pattern o =
  match o.Hashcons.node with
  | On f -> Picture_lib.Dash.On (num f)
  | Off f -> Picture_lib.Dash.Off (num f)

and command' = function
  | CDraw (p, b) ->
      let p = path p in
      let { color = c; pen = pe; dash = dsh } = b.Hashcons.node in
      let pe = (option_def_compile default_pen pen) pe in
      let dsh = (option_compile dash) dsh in
      Picture_lib.stroke_path p c pe dsh
  | CFill (p, c) ->
      let p = path p in
      Picture_lib.fill_path p c
  | CDotLabel (pic, pos, pt) ->
      Picture_lib.on_top
        (Picture_lib.draw_point (point pt))
        (command (mkCLabel pic pos pt))
  | CLabel (pic, pos, pt) ->
      let pic = commandpic pic in
      let pt = point pt in
      let mm = Picture_lib.bounding_box pic in
      let anchor = anchor_of_position pos in
      let pos = point_of_position default_labeloffset mm anchor in
      let tr = Matrix.translation (P.sub pt pos) in
      Picture_lib.transform tr pic
  | CExternalImage (filename, sp) ->
      Picture_lib.external_image filename (spec sp)

and spec = function
  | `Exact (n1, n2) -> `Exact (num n1, num n2)
  | `Height n -> `Height (num n)
  | `Width n -> `Width (num n)
  | `Inside (n1, n2) -> `Inside (num n1, num n2)
  | `None -> `None

and pen p =
  (* TODO : the bounding box is not aware of the pen size *)
  match p.Hashcons.node with
  | PenCircle -> Matrix.identity
  | PenSquare ->
      (*TODO not with cairo...*) assert false (*Picture_lib.PenSquare*)
  | PenFromPath _p ->
      (*TODO : very hard*) assert false (*Picture_lib.PenFromPath (path p)*)
  | PenTransformed (p, tr) -> Matrix.multiply (transform tr) (pen p)

and command c = memoize command' "command" command_memoize c

let commandl_error ferror arg = List.map (ferror command) arg

let commandpicl_error ferror arg = List.map (ferror commandpic) arg

let numl_error ferror arg = List.map (ferror num) arg

let pointl_error ferror arg = List.map (ferror point) arg

let pathl_error ferror arg = List.map (ferror path) arg

let metapathl_error ferror arg = List.map (ferror metapath) arg

let picturel_error ferror arg = List.map (ferror picture) arg
