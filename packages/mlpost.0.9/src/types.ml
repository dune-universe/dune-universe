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

type name = string

include Concrete_types

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

and on_off_node = On of num | Off of num

and on_off = on_off_node hash_consed

and direction_node = Vec of point | Curl of float | NoDir

and direction = direction_node hash_consed

and joint_node =
  | JLine
  | JCurve
  | JCurveNoInflex
  | JTension of float * float
  | JControls of point * point

and joint = joint_node hash_consed

and knot_node = { knot_in : direction; knot_p : point; knot_out : direction }

and knot = knot_node hash_consed

and metapath_node =
  | MPAConcat of knot * joint * metapath
  | MPAKnot of knot
  | MPAAppend of metapath * joint * metapath
  | MPAofPA of path

(*| MPATransformed of metapath * transform*)
and metapath = metapath_node hash_consed

and path_node =
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

and dash_node =
  | DEvenly
  | DWithdots
  | DScaled of num * dash
  | DShifted of point * dash
  | DPattern of on_off list

and dash = dash_node hash_consed

and pen_node =
  | PenCircle
  | PenSquare
  | PenFromPath of path
  | PenTransformed of pen * transform

and pen = pen_node hash_consed

and picture_node =
  | PITex of string
  | PITransformed of commandpic * transform
  | PIClip of commandpic * path

and picture = picture_node hash_consed

and command_node =
  | CDraw of path * brush
  | CFill of path * color option
  | CLabel of commandpic * position * point
  | CDotLabel of commandpic * position * point
  | CExternalImage of string * spec_image

and spec_image =
  [ `None
  | `Width of num (* keep the proportion of the image *)
  | `Height of num
  | `Inside of num * num (* must be inside a box of this height and width *)
  | `Exact of num * num ]

and command = command_node hash_consed

and commandpic_node =
  | Picture of picture
  | Command of command
  | Seq of commandpic list

and commandpic = commandpic_node hash_consed

and brush_node = { pen : pen option; dash : dash option; color : color option }

and brush = brush_node hash_consed

let hash_float : float -> int = Hashtbl.hash

let hash_string : string -> int = Hashtbl.hash

let combine n acc = (acc * 65599) + n

let combine2 n acc1 acc2 = combine n (combine acc1 acc2)

let combine3 n acc1 acc2 acc3 = combine n (combine acc1 (combine acc2 acc3))

let combine4 n acc1 acc2 acc3 acc4 = combine n (combine3 acc1 acc2 acc3 acc4)

let num = hash_float

let point { Point_lib.x; y } = combine2 12 (num x) (num y)

let transform' { Matrix.xx; yx; xy; yy; x0; y0 } =
  combine4 52 (hash_float xx) (hash_float yx) (hash_float xy)
    (combine3 53 (hash_float yy) (hash_float x0) (hash_float y0))

let rec hash_list h l =
  match l with [] -> 54 | x :: xs -> combine2 55 (h x) (hash_list h xs)

let transform = hash_list transform'

let on_off = function On n -> combine 65 (num n) | Off n -> combine 66 (num n)

let direction = function
  | Vec p -> combine 61 (point p)
  | Curl f -> combine 62 (hash_float f)
  | NoDir -> 63

let joint = function
  | JLine -> 67
  | JCurve -> 68
  | JCurveNoInflex -> 69
  | JTension (f1, f2) -> combine2 70 (hash_float f1) (hash_float f2)
  | JControls (p1, p2) -> combine2 71 (point p1) (point p2)

let knot k = combine3 64 k.knot_in.hkey (point k.knot_p) k.knot_out.hkey

let metapath = function
  | MPAConcat (k, j, p) -> combine3 85 k.hkey j.hkey p.hkey
  | MPAAppend (p1, j, p2) -> combine3 86 p1.hkey j.hkey p2.hkey
  | MPAofPA p -> combine 87 p.hkey
  | MPAKnot k -> combine 88 k.hkey

let path = function
  | PAofMPA p -> combine 89 p.hkey
  | MPACycle (d, j, p) -> combine3 90 d.hkey j.hkey p.hkey
  | PAFullCircle -> 24
  | PAHalfCircle -> 25
  | PAQuarterCircle -> 26
  | PAUnitSquare -> 27
  | PATransformed (p, tr) -> combine2 28 p.hkey (transform tr)
  | PACutAfter (p, q) -> combine2 32 p.hkey q.hkey
  | PACutBefore (p, q) -> combine2 33 p.hkey q.hkey
  | PABuildCycle l -> List.fold_left (fun acc p -> combine2 35 acc p.hkey) 34 l
  | PASub (f1, f2, p) -> combine3 36 (hash_float f1) (hash_float f2) p.hkey
  | PABBox p -> combine 37 p.hkey

let picture = function
  | PITex s -> combine 38 (hash_string s)
  | PITransformed (p, tr) -> combine2 40 p.hkey (transform tr)
  | PIClip (p, q) -> combine2 42 p.hkey q.hkey

let commandpic = function
  | Picture pic -> combine 91 pic.hkey
  | Command c -> combine 92 c.hkey
  | Seq l -> List.fold_left (fun acc t -> combine2 93 acc t.hkey) 94 l

let dash = function
  | DEvenly -> 72
  | DWithdots -> 73
  | DScaled (f, d) -> combine2 74 (num f) d.hkey
  | DShifted (p, d) -> combine2 75 (point p) d.hkey
  | DPattern l -> List.fold_left (fun acc o -> combine2 76 acc o.hkey) 77 l

let pen = function
  | PenCircle -> 78
  | PenSquare -> 79
  | PenFromPath p -> combine 80 p.hkey
  | PenTransformed (p, tr) -> combine2 81 p.hkey (transform tr)

let hash_opt f = function None -> 83 | Some o -> combine 84 (f o)

let hash_key x = x.hkey

let hash_color = Hashtbl.hash

let hash_position = Hashtbl.hash

let hash_spec_image = Hashtbl.hash

let command = function
  | CDraw (pa, b) -> combine2 43 pa.hkey b.hkey
  | CFill (p, c) -> combine2 46 p.hkey (hash_color c)
  | CLabel (pic, pos, poi) ->
      combine3 47 pic.hkey (hash_position pos) (point poi)
  | CDotLabel (pic, pos, poi) ->
      combine3 48 pic.hkey (hash_position pos) (point poi)
  | CExternalImage (filename, spec) ->
      combine2 52 (hash_string filename) (hash_spec_image spec)

let brush b =
  combine3 85
    (hash_opt hash_color b.color)
    (hash_opt hash_key b.pen) (hash_opt hash_key b.dash)

(** equality *)

(* equality of floats with correct handling of nan *)
let eq_float (f1 : float) (f2 : float) = Stdlib.compare f1 f2 == 0

let eq_num = eq_float

let eq_point (a : point) (b : point) = Stdlib.compare a b == 0

let eq_matrix (a : matrix) (b : matrix) = Stdlib.compare a b == 0

(* we enforce to use physical equality only on hash-consed data
   of the same type *)
let eq_hashcons (x : 'a hash_consed) (y : 'a hash_consed) = x == y

let rec eq_hashcons_list (x : 'a hash_consed list) (y : 'a hash_consed list) =
  match (x, y) with
  | [], [] -> true
  | h1 :: t1, h2 :: t2 -> h1 == h2 && eq_hashcons_list t1 t2
  | _ -> false

let rec eq_list eq x y =
  match (x, y) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && eq_list eq xs ys
  | _, _ -> false

let eq_transform = eq_list eq_matrix

let eq_opt f o1 o2 =
  match (o1, o2) with
  | None, None -> true
  | Some x1, Some x2 -> f x1 x2
  | _ -> false

let eq_color c1 c2 = Stdlib.compare c1 c2 = 0

let eq_pen_node p1 p2 =
  match (p1, p2) with
  | PenCircle, PenCircle | PenSquare, PenSquare -> true
  | PenFromPath p1, PenFromPath p2 -> eq_hashcons p1 p2
  | PenTransformed (p1, tr1), PenTransformed (p2, tr2) ->
      eq_hashcons p1 p2 && eq_transform tr1 tr2
  | _ -> false

let eq_dash_node d1 d2 =
  match (d1, d2) with
  | DEvenly, DEvenly | DWithdots, DWithdots -> true
  | DScaled (f1, d1), DScaled (f2, d2) -> eq_num f1 f2 && eq_hashcons d1 d2
  | DShifted (p1, d1), DShifted (p2, d2) -> eq_point p1 p2 && eq_hashcons d1 d2
  | DPattern l1, DPattern l2 -> eq_hashcons_list l1 l2
  | _ -> false

let eq_brush_node b1 b2 =
  eq_opt eq_color b1.color b2.color
  && eq_opt eq_hashcons b1.pen b2.pen
  && eq_opt eq_hashcons b1.dash b2.dash

let eq_on_off o1 o2 =
  match (o1, o2) with
  | Off n1, Off n2 | On n1, On n2 -> eq_num n1 n2
  | _ -> false

let eq_position (p1 : position) (p2 : position) = p1 == p2

(* correct because this type contains only constants *)

let eq_metapath_node p1 p2 =
  match (p1, p2) with
  | MPAConcat (k1, j1, p1), MPAConcat (k2, j2, p2) ->
      eq_hashcons k1 k2 && eq_hashcons j1 j2 && eq_hashcons p1 p2
  | MPAKnot k1, MPAKnot k2 -> eq_hashcons k1 k2
  | MPAAppend (p11, j1, p12), MPAAppend (p21, j2, p22) ->
      eq_hashcons p11 p21 && eq_hashcons j1 j2 && eq_hashcons p12 p22
  | MPAofPA p1, MPAofPA p2 -> eq_hashcons p1 p2
  | _ -> false

let eq_path_node p1 p2 =
  match (p1, p2) with
  | PAofMPA p1, PAofMPA p2 -> eq_hashcons p1 p2
  | MPACycle (d1, j1, p1), MPACycle (d2, j2, p2) ->
      eq_hashcons d1 d2 && eq_hashcons j1 j2 && eq_hashcons p1 p2
  | PAFullCircle, PAFullCircle
  | PAHalfCircle, PAHalfCircle
  | PAQuarterCircle, PAQuarterCircle
  | PAUnitSquare, PAUnitSquare ->
      true
  | PATransformed (p1, tr1), PATransformed (p2, tr2) ->
      eq_hashcons p1 p2 && eq_transform tr1 tr2
  | PACutAfter (p11, p12), PACutAfter (p21, p22)
  | PACutBefore (p11, p12), PACutBefore (p21, p22) ->
      eq_hashcons p11 p21 && eq_hashcons p12 p22
  | PABuildCycle l1, PABuildCycle l2 -> eq_hashcons_list l1 l2
  | PASub (f11, f12, p1), PASub (f21, f22, p2) ->
      eq_num f11 f21 && eq_num f12 f22 && eq_hashcons p1 p2
  | PABBox p1, PABBox p2 -> eq_hashcons p1 p2
  | _ -> false

let eq_picture_node p1 p2 =
  match (p1, p2) with
  | PITex s1, PITex s2 ->
      (* it actually happens that the same text appears twice *)
      s1 <> "" && s1 = s2
  | PITransformed (p1, tr1), PITransformed (p2, tr2) ->
      eq_hashcons p1 p2 && eq_transform tr1 tr2
  | PIClip (pi1, pa1), PIClip (pi2, pa2) ->
      eq_hashcons pi1 pi2 && eq_hashcons pa1 pa2
  | _ -> false

let eq_knot_node k1 k2 =
  eq_hashcons k1.knot_in k2.knot_in
  && eq_point k1.knot_p k2.knot_p
  && eq_hashcons k1.knot_out k2.knot_out

let eq_joint_node j1 j2 =
  match (j1, j2) with
  | JLine, JLine | JCurve, JCurve | JCurveNoInflex, JCurveNoInflex -> true
  | JTension (f11, f12), JTension (f21, f22) ->
      eq_float f11 f21 && eq_float f12 f22
  | JControls (p11, p12), JControls (p21, p22) ->
      eq_point p11 p21 && eq_point p12 p22
  | _ -> false

let eq_direction_node d1 d2 =
  match (d1, d2) with
  | Vec p1, Vec p2 -> eq_point p1 p2
  | Curl f1, Curl f2 -> eq_float f1 f2
  | NoDir, NoDir -> true
  | _ -> false

let eq_command_node c1 c2 =
  match (c1, c2) with
  | CDraw (p1, b1), CDraw (p2, b2) -> eq_hashcons p1 p2 && eq_hashcons b1 b2
  | CFill (p1, c1), CFill (p2, c2) -> eq_hashcons p1 p2 && eq_opt eq_color c1 c2
  | CLabel (pic1, pos1, poi1), CLabel (pic2, pos2, poi2)
  | CDotLabel (pic1, pos1, poi1), CDotLabel (pic2, pos2, poi2) ->
      eq_hashcons pic1 pic2 && eq_position pos1 pos2 && eq_point poi1 poi2
  | _ -> false

let eq_commandpic_node p1 p2 =
  match (p1, p2) with
  | Picture p1, Picture p2 -> eq_hashcons p1 p2
  | Command p1, Command p2 -> eq_hashcons p1 p2
  | Seq l1, Seq l2 -> eq_hashcons_list l1 l2
  | _ -> false

(* smart constructors *)

(* num *)

let unsigned f x = f x land 0x3FFFFFFF

(* knot *)

module HashKnot = Hashcons.Make (struct
  type t = knot_node

  let equal = eq_knot_node

  let hash = unsigned knot
end)

let hashknot_table = HashKnot.create 257

let hashknot = HashKnot.hashcons hashknot_table

let mkKnot d1 p d2 = hashknot { knot_in = d1; knot_p = p; knot_out = d2 }

(* metapath *)
module HashMetaPath = Hashcons.Make (struct
  type t = metapath_node

  let equal = eq_metapath_node

  let hash = unsigned metapath
end)

let hashmetapath_table = HashMetaPath.create 257

let hashmetapath = HashMetaPath.hashcons hashmetapath_table

let mkMPAKnot k = hashmetapath (MPAKnot k)

let mkMPAConcat k j p2 = hashmetapath (MPAConcat (k, j, p2))

let mkMPAAppend x y z = hashmetapath (MPAAppend (x, y, z))

let mkMPAofPA p = hashmetapath (MPAofPA p)

(*val mkMPATransformed : path -> transform -> path*)

(* path *)

module HashPath = Hashcons.Make (struct
  type t = path_node

  let equal = eq_path_node

  let hash = unsigned path
end)

let hashpath_table = HashPath.create 257

let hashpath = HashPath.hashcons hashpath_table

let mkPAofMPA p = hashpath (PAofMPA p)

let mkPAKnot k = mkPAofMPA (mkMPAKnot k)

let mkPAConcat k j p2 = mkPAofMPA (mkMPAConcat k j (mkMPAofPA p2))

let mkMPACycle d j p1 = hashpath (MPACycle (d, j, p1))

let mkPACycle d j p1 = mkMPACycle d j (mkMPAofPA p1)

let mkPAAppend x y z = mkPAofMPA (mkMPAAppend (mkMPAofPA x) y (mkMPAofPA z))

let mkPAFullCircle = hashpath PAFullCircle

let mkPAHalfCircle = hashpath PAHalfCircle

let mkPAQuarterCircle = hashpath PAQuarterCircle

let mkPAUnitSquare = hashpath PAUnitSquare

let mkPATransformed x y = hashpath (PATransformed (x, y))

let mkPACutAfter x y = hashpath (PACutAfter (x, y))

let mkPACutBefore x y = hashpath (PACutBefore (x, y))

let mkPABuildCycle l = hashpath (PABuildCycle l)

let mkPASub x y z = hashpath (PASub (x, y, z))

let mkPABBox pic = hashpath (PABBox pic)

(* joint *)

module HashJoint = Hashcons.Make (struct
  type t = joint_node

  let equal = eq_joint_node

  let hash = unsigned joint
end)

let hashjoint_table = HashJoint.create 257

let hashjoint = HashJoint.hashcons hashjoint_table

let mkJCurve = hashjoint JCurve

let mkJLine = hashjoint JLine

let mkJCurveNoInflex = hashjoint JCurveNoInflex

let mkJTension x y = hashjoint (JTension (x, y))

let mkJControls x y = hashjoint (JControls (x, y))

(* direction *)

module HashDir = Hashcons.Make (struct
  type t = direction_node

  let equal = eq_direction_node

  let hash = unsigned direction
end)

let hashdir_table = HashDir.create 257

let hashdir = HashDir.hashcons hashdir_table

let mkNoDir = hashdir NoDir

let mkVec p = hashdir (Vec p)

let mkCurl f = hashdir (Curl f)

(* picture *)

module HashPicture = Hashcons.Make (struct
  type t = picture_node

  let equal = eq_picture_node

  let hash = unsigned picture
end)

let hashpicture_table = HashPicture.create 257

let hashpicture = HashPicture.hashcons hashpicture_table

let mkPITex s = hashpicture (PITex s)

let mkPITransformed x y = hashpicture (PITransformed (x, y))

let mkPIClip p pic = hashpicture (PIClip (p, pic))

(* command *)

module HashCommand = Hashcons.Make (struct
  type t = command_node

  let equal = eq_command_node

  let hash = unsigned command
end)

let hashcommand_table = HashCommand.create 257

let hashcommand = HashCommand.hashcons hashcommand_table

let mkCDraw x y = hashcommand (CDraw (x, y))

let mkCFill x y = hashcommand (CFill (x, y))

let mkCLabel x y z = hashcommand (CLabel (x, y, z))

let mkCDotLabel x y z = hashcommand (CDotLabel (x, y, z))

let mkCExternalImage f s = hashcommand (CExternalImage (f, s))

(* commandPic *)
module HashCommandPic = Hashcons.Make (struct
  type t = commandpic_node

  let equal = eq_commandpic_node

  let hash = unsigned commandpic
end)

let hashcommandpic_table = HashCommandPic.create 257

let hashcommandpic = HashCommandPic.hashcons hashcommandpic_table

let mkPicture p = hashcommandpic (Picture p)

let mkCommand p = hashcommandpic (Command p)

let mkSeq l = hashcommandpic (Seq l)

(* dash *)

module HashDash = Hashcons.Make (struct
  type t = dash_node

  let equal = eq_dash_node

  let hash = unsigned dash
end)

let hashdash_table = HashDash.create 257

let hashdash = HashDash.hashcons hashdash_table

let mkDEvenly = hashdash DEvenly

let mkDWithdots = hashdash DWithdots

let mkDScaled x y = hashdash (DScaled (x, y))

let mkDShifted x y = hashdash (DShifted (x, y))

let mkDPattern l = hashdash (DPattern l)

(* pen *)

module HashPen = Hashcons.Make (struct
  type t = pen_node

  let equal = eq_pen_node

  let hash = unsigned pen
end)

let hashpen_table = HashPen.create 257

let hashpen = HashPen.hashcons hashpen_table

let mkPenCircle = hashpen PenCircle

let mkPenSquare = hashpen PenSquare

let mkPenFromPath p = hashpen (PenFromPath p)

let mkPenTransformed x y = hashpen (PenTransformed (x, y))

(* brush *)
module HashBrush = Hashcons.Make (struct
  type t = brush_node

  let equal = eq_brush_node

  let hash = unsigned brush
end)

let hashbrush_table = HashBrush.create 257

let hashbrush = HashBrush.hashcons hashbrush_table

let mkBrush c p d = hashbrush { pen = p; color = c; dash = d }

let opt_def_node def = function None -> def | Some s -> s.node

let opt_def_map f def = function None -> def | Some s -> f s

let mkBrushOpt b c p d =
  let b = opt_def_node { color = None; pen = None; dash = None } b in
  let b = opt_def_map (fun x -> { b with color = Some x }) b c in
  let b = opt_def_map (fun x -> { b with pen = Some x }) b p in
  let b = opt_def_map (fun x -> { b with dash = Some x }) b d in
  hashbrush b

(* on_off *)

module HashOnOff = Hashcons.Make (struct
  type t = on_off_node

  let equal = eq_on_off

  let hash = unsigned on_off
end)

let hashon_off_table = HashOnOff.create 257

let hashon_off = HashOnOff.hashcons hashon_off_table

let mkOn n = hashon_off (On n)

let mkOff n = hashon_off (Off n)

let hreduce = function
  | `Center -> `Center
  | `Left | `West -> `West
  | `Right | `East -> `East

let vreduce = function
  | `Center -> `Center
  | `Top | `North -> `North
  | `Bot | `Bottom | `South -> `South

let corner_reduce = function
  | `Upleft | `Upperleft | `Topleft | `Northwest -> `Northwest
  | `Upright | `Upperright | `Topright | `Northeast -> `Northeast
  | `Lowleft | `Lowerleft | `Bottomleft | `Southwest -> `Southwest
  | `Lowright | `Lowerright | `Bottomright | `Southeast -> `Southeast

let pos_reduce = function
  | #hposition as p -> hreduce p
  | #vposition as p -> vreduce p
  | #corner as p -> corner_reduce p

let opposite_position (x : position) : position_red =
  match pos_reduce x with
  | `Center -> `Center
  | `West -> `East
  | `East -> `West
  | `North -> `South
  | `South -> `North
  | `Northwest -> `Southeast
  | `Northeast -> `Southwest
  | `Southwest -> `Northeast
  | `Southeast -> `Northwest
