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

(* print an mlpost AST for debugging *)
open Concrete_types
open Types
open Format

let option_print pr fmt = function None -> () | Some x -> pr fmt x

let position fmt p =
  match pos_reduce p with
  | `Center -> fprintf fmt ""
  | `West -> fprintf fmt ".lft"
  | `East -> fprintf fmt ".rt"
  | `North -> fprintf fmt ".top"
  | `South -> fprintf fmt ".bot"
  | `Northwest -> fprintf fmt ".ulft"
  | `Northeast -> fprintf fmt ".urt"
  | `Southwest -> fprintf fmt ".llft"
  | `Southeast -> fprintf fmt ".lrt"

let rec num fmt f = pp_print_float fmt f

and point fmt { Point_lib.x; y } = fprintf fmt "(%a,%a)" num x num y

and picture fmt p =
  match p.Hashcons.node with
  | PITex s -> fprintf fmt "tex(%s)" s
  | PITransformed (p, tr) ->
      fprintf fmt "%a transformed %a" commandpic p transform tr
  | PIClip _ -> ()

and transform fmt t = Misc.print_list Misc.comma Matrix.print fmt t

and knot fmt k =
  match k.Hashcons.node with
  | { knot_in = d1; knot_p = p; knot_out = d2 } ->
      fprintf fmt "%a%a%a" direction d1 point p direction d2

and direction fmt d =
  match d.Hashcons.node with
  | Vec p -> fprintf fmt "{%a}" point p
  | Curl f -> fprintf fmt "{curl %f}" f
  | NoDir -> ()

and metapath fmt p =
  match p.Hashcons.node with
  | MPAConcat (k, j, p) -> fprintf fmt "%a%a%a" metapath p joint j knot k
  | MPAAppend (p1, j, p2) ->
      fprintf fmt "%a%a%a" metapath p1 joint j metapath p2
  | MPAKnot k -> knot fmt k
  | MPAofPA p -> fprintf fmt "(%a)" path p

and path fmt p =
  match p.Hashcons.node with
  | PAofMPA p -> fprintf fmt "(from_metapath %a)" metapath p
  | MPACycle (_, j, p) -> fprintf fmt "(cycle of %a with %a)" metapath p joint j
  | PATransformed (p, tr) -> fprintf fmt "(tr %a by %a)" path p transform tr
  | PACutAfter (p1, p2) -> fprintf fmt "(cutafter %a by %a)" path p1 path p2
  | PACutBefore (p1, p2) -> fprintf fmt "(cutbefore %a by %a)" path p1 path p2
  | PABuildCycle pl ->
      fprintf fmt "(buildcycle %a)" (Misc.print_list Misc.semicolon path) pl
  | PASub (f1, f2, p) ->
      fprintf fmt "(sub %a from %a to %a)" path p num f1 num f2
  | PABBox p -> fprintf fmt "(bbox %a)" commandpic p
  | PAUnitSquare -> fprintf fmt "unitsquare"
  | PAQuarterCircle -> fprintf fmt "quartercircle"
  | PAHalfCircle -> fprintf fmt "halfcircle"
  | PAFullCircle -> fprintf fmt "fullcircle"

and joint fmt j =
  match j.Hashcons.node with
  | JLine -> fprintf fmt "--"
  | JCurve -> fprintf fmt ".."
  | JCurveNoInflex -> fprintf fmt "..."
  | JTension (a, b) -> fprintf fmt "..tension(%f,%f).." a b
  | JControls (p1, p2) -> fprintf fmt "..%a..%a.." point p1 point p2

and command fmt c =
  match c.Hashcons.node with
  | CDraw (p, b) ->
      let { color = c; pen = pe; dash = d } = b.Hashcons.node in
      fprintf fmt "draw (%a,%a,%a,%a);" path p option_color c option_pen pe
        option_dash d
  | CFill (p, c) -> fprintf fmt "fill (%a,%a);" path p option_color c
  | CLabel (pic, pos, pt) ->
      fprintf fmt "label%a(%a,%a)" position pos commandpic pic point pt
  | CDotLabel (pic, pos, pt) ->
      fprintf fmt "dotlabel%a(%a,%a)" position pos commandpic pic point pt
  | CExternalImage (f, _) -> fprintf fmt "externalimage %s@ " f

and color fmt (c : Types.color) =
  let mode, color =
    match c with OPAQUE c -> (None, c) | TRANSPARENT (f, c) -> (Some f, c)
  in
  let pmode fmt = function
    | None -> fprintf fmt "O"
    | Some f -> fprintf fmt "%f" f
  in
  match color with
  | RGB (r, g, b) -> fprintf fmt "(%f, %f , %f, %a)" r g b pmode mode
  | CMYK (c, m, y, k) -> fprintf fmt "(%f, %f, %f, %f, %a)" c m y k pmode mode
  | Gray f -> fprintf fmt "(%f * white, %a)" f pmode mode

and commandpic fmt c =
  match c.Hashcons.node with
  | Picture p -> picture fmt p
  | Command c -> command fmt c
  | Seq l -> Misc.print_list Misc.space commandpic fmt l

and pen fmt x =
  match x.Hashcons.node with
  | PenCircle -> fprintf fmt "PenCircle"
  | PenSquare -> assert false
  | PenFromPath _ -> assert false
  | PenTransformed (p, trl) -> fprintf fmt "(%a,%a)" pen p transform trl

and dash fmt x =
  match x.Hashcons.node with
  | DEvenly -> fprintf fmt "evenly"
  | DWithdots -> assert false
  | DScaled (f, d) -> fprintf fmt "%a scaled %a" dash d num f
  | DShifted (_, _) -> assert false
  | DPattern _ -> assert false

and option_pen fmt = option_print pen fmt

and option_color fmt = option_print color fmt

and option_dash fmt = option_print dash fmt
