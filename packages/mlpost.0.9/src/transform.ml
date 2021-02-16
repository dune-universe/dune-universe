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

type t' = matrix

type matrix = t'

type t = t' list

let scaled = Matrix.scale

let rotated f = Matrix.rotation (Num.deg2rad f)

let shifted = Matrix.translation

let slanted = Matrix.slanted

let xscaled = Matrix.xscaled

let yscaled = Matrix.yscaled

let zscaled = Matrix.zscaled

let reflect = Matrix.reflect

let rotate_around p f = Matrix.rotate_around p (Num.deg2rad f)

let explicit t = t

(* applied the transformations in the order of the list *)
let id = []
