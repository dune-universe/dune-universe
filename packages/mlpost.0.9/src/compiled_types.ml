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

type num = float

and point = Point_lib.t

and direction = Vec of point | Curl of float | NoDir

and joint =
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

and knot = direction * point * direction

and path =
  | PAScope of path
  | PAConcat of knot * joint * path
  | PACycle of direction * joint * path
  | PAFullCircle
  | PAHalfCircle
  | PAQuarterCircle
  | PAUnitSquare
  | PATransformed of path * transform
  | PAKnot of knot
  | PAAppend of path * joint * path
  | PACutAfter of path * path
  | PACutBefore of path * path
  | PABuildCycle of path list
  (* PASub only takes a name *)
  | PASub of num * num * name
  | PABBox of picture
  | PAName of name

and matrix = Types.matrix

and transform = name

and picture =
  | PITex of string
  | PITransformed of picture * transform
  | PIName of name

and dash =
  | DEvenly
  | DWithdots
  | DScaled of num * dash
  | DShifted of point * dash
  | DPattern of on_off list

and pen =
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform

and command =
  | CDraw of path * color option * pen option * dash option
  | CDrawArrow of path * color option * pen option * dash option
  | CDrawPic of picture
  | CFill of path * color option
  | CLabel of picture * position * point
  | CDotLabel of picture * position * point
  | CSeq of command list
  | CDeclPath of name * path
  | CDeclPoint of name * point
  | CDeclNum of name * num
  | CDefPic of name * command
  | CDefTrans of name * matrix
  | CSimplePic of name * picture
  | CClip of name * path
  | CExternalImage of string * spec_image

and spec_image =
  [ `None
  | `Width of num (* keep the proportion of the image *)
  | `Height of num
  | `Inside of num * num (* must be inside a box of this height and width *)
  | `Exact of num * num ]

and color = Types.color

and position = Types.position

and name = Types.name

and on_off = On of num | Off of num
