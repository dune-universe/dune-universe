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

(*
type color =
  | RGB of float * float * float
  | CMYK of float * float * float * float
  | Gray of float

type name = string

type corner = N | S | W | E | NE | NW | SW | SE
type piccorner = UL | UR | LL | LR
*)

open Hashtbl
open Types

let combine n acc = (acc * 65599) + n

let combine2 n acc1 acc2 = combine n (combine acc1 acc2)

let combine3 n acc1 acc2 acc3 = combine n (combine acc1 (combine acc2 acc3))

let combine4 n acc1 acc2 acc3 acc4 = combine n (combine3 acc1 acc2 acc3 acc4)

type position =
  | Pcenter
  | Pleft
  | Pright
  | Ptop
  | Pbot
  | Pupleft
  | Pupright
  | Plowleft
  | Plowright

let rec num = function
  | F f -> combine 1 (hash f)
  | NXPart p -> combine 2 (point p)
  | NYPart p -> combine 3 (point p)
  | NAdd (n, m) -> combine2 4 (num n) (num m)
  | NMinus (n, m) -> combine2 5 (num n) (num m)
  | NMult (n, m) -> combine2 6 (num n) (num m)
  | NDiv (n, m) -> combine2 7 (num n) (num m)
  | NMax (n, m) -> combine2 8 (num n) (num m)
  | NMin (n, m) -> combine2 9 (num n) (num m)
  | NGMean (n, m) -> combine2 10 (num n) (num m)
  | NLength p -> combine 11 (path p)

and point = function
  | PTPair (n, m) -> combine2 12 (num n) (num m)
  | PTPicCorner (p, pc) -> combine2 13 (picture p) (hash pc)
  | PTPointOf (f, p) -> combine2 14 (hash f) (path p)
  | PTDirectionOf (f, p) -> combine2 15 (hash f) (path p)
  | PTAdd (p, q) -> combine2 16 (point p) (point q)
  | PTSub (p, q) -> combine2 17 (point p) (point q)
  | PTMult (n, q) -> combine2 18 (num n) (point q)
  | PTRotated (f, p) -> combine2 19 (hash f) (point p)
  | PTTransformed (p, l) ->
      List.fold_left
        (fun acc t -> combine2 21 acc (transform t))
        (combine 20 (point p))
        l

(*
and on_off = On of num | Off of num
*)
and direction = function
  | Vec p -> combine 61 (point p)
  | Curl f -> combine 62 (hash f)
  | NoDir -> 63

and joint = hash

(*
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point
*)
and knot (d1, p, d2) = combine3 64 (direction d1) (point p) (direction d2)

and path = function
  | PAConcat (k, j, p) -> combine3 22 (knot k) (joint j) (path p)
  | PACycle (d, j, p) -> combine3 23 (direction d) (joint j) (path p)
  | PAFullCircle -> 24
  | PAHalfCircle -> 25
  | PAQuarterCircle -> 26
  | PAUnitSquare -> 27
  | PATransformed (p, l) ->
      List.fold_left
        (fun acc t -> combine2 28 acc (transform t))
        (combine 29 (path p))
        l
  | PAKnot k -> combine 30 (knot k)
  | PAAppend (p1, j, p2) -> combine3 31 (path p1) (joint j) (path p2)
  | PACutAfter (p, q) -> combine2 32 (path p) (path q)
  | PACutBefore (p, q) -> combine2 33 (path p) (path q)
  | PABuildCycle l ->
      List.fold_left (fun acc t -> combine2 35 acc (path t)) 34 l
  | PASub (f1, f2, p) -> combine3 36 (hash f1) (hash f2) (path p)
  | PABBox p -> combine 37 (picture p)

and transform = function
  | TRRotated f -> combine 52 (hash f)
  | TRScaled n -> combine 53 (num n)
  | TRShifted p -> combine 57 (point p)
  | TRSlanted n -> combine 54 (num n)
  | TRXscaled n -> combine 55 (num n)
  | TRYscaled n -> combine 56 (num n)
  | TRZscaled p -> combine 58 (point p)
  | TRReflect (p, q) -> combine2 59 (point p) (point q)
  | TRRotateAround (p, q) -> combine2 60 (point p) (hash q)

and picture = function
  | PITex s -> combine 38 (hash s)
  | PIMake c -> combine 39 (command c)
  | PITransform (l, p) ->
      List.fold_left
        (fun acc t -> combine2 40 acc (transform t))
        (combine 41 (picture p))
        l
  | PIClip (p, q) -> combine2 42 (picture p) (path q)

and dash = hash

(*
  | DEvenly
  | DWithdots
  | DScaled of float * dash
  | DShifted of point * dash
  | DPattern of on_off list
*)
and pen = hash

(*
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform list
*)
and command = function
  | CDraw (pa, c, p, d) -> combine4 43 (path pa) (hash c) (hash p) (hash d)
  | CDrawArrow (pa, c, p, d) -> combine4 44 (path pa) (hash c) (hash p) (hash d)
  | CDrawPic p -> combine 45 (picture p)
  | CFill (p, c) -> combine2 46 (path p) (hash c)
  | CLabel (pic, pos, poi) -> combine3 47 (picture pic) (hash pos) (point poi)
  | CDotLabel (pic, pos, poi) ->
      combine3 48 (picture pic) (hash pos) (point poi)
  | CLoop (n, m, _) -> combine2 49 n m
  | CSeq l -> List.fold_left (fun acc t -> combine2 50 acc (command t)) 51 l
