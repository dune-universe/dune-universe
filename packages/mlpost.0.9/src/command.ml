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
module T = Transform

type hposition = Types.hposition

type vposition = Types.vposition

type position = Types.position

type t = commandpic

let label ?(pos = `Center) pic point = mkCommand (mkCLabel pic pos point)

(* replace later *)
let dotlabel ?(pos = `Center) pic point = mkCommand (mkCDotLabel pic pos point)

let draw ?brush ?color ?pen ?dashed t =
  (* We don't use a default to avoid the output of
     ... withcolor (0.00red+0.00green+0.00blue) withpen ....
     for each command in the output file *)
  mkCommand (mkCDraw t (mkBrushOpt brush color pen dashed))

let fill ?color t = mkCommand (mkCFill t color)

let seq l = mkSeq l

let iter from until f =
  let l = Misc.fold_from_to (fun acc i -> f i :: acc) [] from until in
  seq (List.rev l)

let draw_pic p = p

let append c1 c2 = seq [ c1; c2 ]

let ( ++ ) = append

let externalimage filename spec =
  if not (Filename.check_suffix filename "png") then
    invalid_arg
      (Format.sprintf "externalimage support only png image : %s" filename);
  if not (Sys.file_exists filename) then
    invalid_arg
      (Format.sprintf "externalimage file doesn't exist : %s" filename);
  let filename =
    if Filename.is_relative filename then
      Filename.concat (Sys.getcwd ()) filename
    else filename
  in
  mkCommand (mkCExternalImage filename spec)

(* syntactic sugar *)

let iterl f l = seq (List.map f l)

let nop = seq []
