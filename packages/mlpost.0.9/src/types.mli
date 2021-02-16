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

type color = Concrete_types.color

type name = string

type corner =
  [ `Northwest
  | `Northeast
  | `Southwest
  | `Southeast
  | `Upleft
  | `Upright
  | `Lowleft
  | `Lowright
  | `Upperleft
  | `Upperright
  | `Lowerleft
  | `Lowerright
  | `Topleft
  | `Topright
  | `Bottomleft
  | `Bottomright ]

type corner_red = [ `Northwest | `Northeast | `Southwest | `Southeast ]

type hposition = [ `Center | `West | `East | `Left | `Right ]

type vposition =
  [ `Center | `North | `South | `Top | `Bot  (** deprecated *) | `Bottom ]

type hposition_red = [ `Center | `West | `East ]

type vposition_red = [ `Center | `North | `South ]

type position = [ hposition | vposition | corner ]

type position_red = [ hposition_red | vposition_red | corner_red ]

open Hashcons

type num = float

type point = Point_lib.t

and on_off_node = private On of num | Off of num

and on_off = on_off_node hash_consed

and direction_node = private Vec of point | Curl of float | NoDir

and direction = direction_node hash_consed

and joint_node = private
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

and joint = joint_node hash_consed

and knot_node = private {
  knot_in : direction;
  knot_p : point;
  knot_out : direction;
}

and knot = knot_node hash_consed

and metapath_node = private
  | MPAConcat of knot * joint * metapath
  | MPAKnot of knot
  | MPAAppend of metapath * joint * metapath
  | MPAofPA of path

(*| MPATransformed of metapath * transform*)
and metapath = metapath_node hash_consed

and path_node = private
  | PAofMPA of metapath
  | MPACycle of direction * joint * metapath
  | PAFullCircle
  | PAHalfCircle
  | PAQuarterCircle
  | PAUnitSquare
  | PATransformed of path * transform
  | PACutAfter of path * path
  | PACutBefore of path * path
  | PABuildCycle of path list
  | PASub of num * num * path
  | PABBox of commandpic

and path = path_node hash_consed

and matrix = Matrix.t

and transform = Matrix.t list

and dash_node = private
  | DEvenly
  | DWithdots
  | DScaled of num * dash
  | DShifted of point * dash
  | DPattern of on_off list

and dash = dash_node hash_consed

and pen_node = private
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform

and pen = pen_node hash_consed

and picture_node = private
  | PITex of string
  | PITransformed of commandpic * transform
  | PIClip of commandpic * path

and picture = picture_node hash_consed

and command_node = private
  | CDraw of path * brush
  | CFill of path * color option
  | CLabel of commandpic * position * point
  | CDotLabel of commandpic * position * point
  | CExternalImage of string * spec_image

and commandpic_node = private
  | Picture of picture
  | Command of command
  | Seq of commandpic list

and commandpic = commandpic_node hash_consed

and spec_image =
  [ `None
  | `Width of num (* keep the proportion of the image *)
  | `Height of num
  | `Inside of num * num (* must be inside a box of this height and width *)
  | `Exact of num * num ]

and command = command_node hash_consed

and brush_node = { pen : pen option; dash : dash option; color : color option }

and brush = brush_node hash_consed

(* smart constructors *)

(* knot *)
val mkKnot : direction -> point -> direction -> knot

(* metapath *)

val mkMPAKnot : knot -> metapath

val mkMPAConcat : knot -> joint -> metapath -> metapath

val mkMPAAppend : metapath -> joint -> metapath -> metapath

val mkMPAofPA : path -> metapath

(*val mkMPATransformed : path -> transform -> path*)

(* path *)

val mkPAofMPA : metapath -> path

val mkPAKnot : knot -> path

val mkPAConcat : knot -> joint -> path -> path

val mkPACycle : direction -> joint -> path -> path

val mkMPACycle : direction -> joint -> metapath -> path

val mkPAAppend : path -> joint -> path -> path

val mkPAFullCircle : path

val mkPAHalfCircle : path

val mkPAQuarterCircle : path

val mkPAUnitSquare : path

val mkPATransformed : path -> transform -> path

val mkPACutAfter : path -> path -> path

val mkPACutBefore : path -> path -> path

val mkPABuildCycle : path list -> path

val mkPASub : num -> num -> path -> path

val mkPABBox : commandpic -> path

(* joint *)
val mkJCurve : joint

val mkJLine : joint

val mkJCurveNoInflex : joint

val mkJTension : float -> float -> joint

val mkJControls : point -> point -> joint

(* direction *)

val mkNoDir : direction

val mkVec : point -> direction

val mkCurl : float -> direction

(* picture *)

val mkPITex : string -> picture

val mkPITransformed : commandpic -> transform -> picture

val mkPIClip : commandpic -> path -> picture

(* command *)

val mkCDraw : path -> brush -> command

val mkCFill : path -> color option -> command

val mkCLabel : commandpic -> position -> point -> command

val mkCDotLabel : commandpic -> position -> point -> command

val mkCExternalImage : string -> spec_image -> command

(* commandpic *)
val mkPicture : picture -> commandpic

val mkCommand : command -> commandpic

val mkSeq : commandpic list -> commandpic

(* dash *)

val mkDEvenly : dash

val mkDWithdots : dash

val mkDScaled : num -> dash -> dash

val mkDShifted : point -> dash -> dash

val mkDPattern : on_off list -> dash

(* pen *)

val mkPenCircle : pen

val mkPenSquare : pen

val mkPenFromPath : path -> pen

val mkPenTransformed : pen -> transform -> pen

(* brush *)
val mkBrush : color option -> pen option -> dash option -> brush

val mkBrushOpt :
  brush option -> color option -> pen option -> dash option -> brush

(* on_off *)

val mkOn : num -> on_off

val mkOff : num -> on_off

val pos_reduce : position -> position_red

val corner_reduce : corner -> corner_red

val vreduce : vposition -> vposition_red

val hreduce : hposition -> hposition_red

val opposite_position : position -> position_red
