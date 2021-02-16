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
open Hashcons

(* A duplicate analysis - find out the number of times a node is used *)

module MetaPath = struct
  type t = metapath_node hash_consed

  let equal = ( == )

  let hash x = x.hkey
end

module Path = struct
  type t = path_node hash_consed

  let equal = ( == )

  let hash x = x.hkey
end

module Picture = struct
  type t = picture_node hash_consed

  let equal = ( == )

  let hash x = x.hkey
end

module MPthM = Hashtbl.Make (MetaPath)
module PthM = Hashtbl.Make (Path)
module PicM = Hashtbl.Make (Picture)

let path_map = PthM.create 257

let picture_map = PicM.create 257

let test_and_incr_path n =
  try
    incr (PthM.find path_map n);
    true
  with Not_found ->
    PthM.add path_map n (ref 1);
    false

let test_and_incr_pic n =
  try
    incr (PicM.find picture_map n);
    true
  with Not_found ->
    PicM.add picture_map n (ref 1);
    false

let option_count f = function None -> () | Some x -> f x

let rec metapath p =
  match p.Hashcons.node with
  | MPAConcat (_, _, p) -> metapath p
  | MPAAppend (p1, _, p2) ->
      metapath p1;
      metapath p2
  | MPAKnot _ -> ()
  | MPAofPA p -> path p

and path' = function
  | PAofMPA p -> metapath p
  | MPACycle (_, _, p) -> metapath p
  | PATransformed (p, _) -> path p
  | PACutAfter (p1, p2) | PACutBefore (p1, p2) ->
      path p1;
      path p2
  | PABuildCycle pl -> List.iter path pl
  | PASub (_, _, p) -> path p
  | PABBox p -> commandpic p
  | PAUnitSquare | PAQuarterCircle | PAHalfCircle | PAFullCircle -> ()

and path p =
  (*   Format.printf "%a@." Print.path p; *)
  if test_and_incr_path p then () else path' p.node

and picture' = function
  | PITransformed (p, _) -> commandpic p
  | PITex _ -> ()
  | PIClip (pic, pth) ->
      commandpic pic;
      path pth

and picture p = if test_and_incr_pic p then () else picture' p.node

and command c =
  match c.node with
  | CDraw (p, b) ->
      path p;
      brush b
  | CFill (p, _) -> path p
  | CDotLabel (pic, _, _) -> commandpic pic
  | CLabel (pic, _, _) -> commandpic pic
  | CExternalImage _ -> ()

and brush b =
  let b = b.Hashcons.node in
  option_count pen b.pen;
  option_count dash b.dash

and pen p =
  match p.Hashcons.node with
  | PenCircle | PenSquare -> ()
  | PenFromPath p -> path p
  | PenTransformed (p, _) -> pen p

and dash d =
  match d.Hashcons.node with
  | DEvenly | DWithdots -> ()
  | DScaled (_, d) -> dash d
  | DShifted (_, d) -> dash d
  | DPattern l -> List.iter dash_pattern l

and dash_pattern _ = ()

and commandpic p =
  match p.node with
  | Picture p -> picture p
  | Command c -> command c
  | Seq l -> List.iter commandpic l
