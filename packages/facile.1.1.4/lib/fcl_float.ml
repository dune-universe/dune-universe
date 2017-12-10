(***********************************************************************)
(*                                                                     *)
(*                           FaCiLe                                    *)
(*                 A Functional Constraint Library                     *)
(*                                                                     *)
(*            Nicolas Barnier, Pascal Brisset, LOG, CENA               *)
(*                                                                     *)
(* Copyright 2004 CENA. All rights reserved. This file is distributed  *)
(* under the terms of the GNU Lesser General Public License.           *)
(***********************************************************************)
(* $Id: fcl_float.ml,v 1.3 2003/06/11 14:43:58 barnier Exp $ *)

let epsilon = 1e-3

type elt = float

type t = { min : float; max : float }

let fprint_elt c x =
  Printf.fprintf c "%.3f(%f)" x epsilon

let fprint c x =
  Printf.fprintf c "%.3f..%.3f" x.min x.max

let size x = truncate ((x.max -. x.min) /. epsilon)
let min x = x.min
let max x = x.max
let min_max x = (x.min, x.max)
let mem x f = f.min < x && x < f.max
let interval x y =
  if (y -. x) /. epsilon >= 2. ** 30. then
    Fcl_debug.fatal_error "FloatDomain.interval: range too big";
  { min = x ; max = y }
let included x y = y.min < x.min && x.max < y.max

let strictly_inf x y = x < y

let zero x = abs_float x < epsilon

let compare_elt = compare

(* ca va pas du tout : il faudrait un constructeur pour le cas nul *)
let empty = {min = max_float; max = min_float}

let remove_low x d =
  if x < d.min then d
  else if d.max < x then empty
  else {min = x; max = d.max}

let remove_up x d =
  if x > d.max then d
  else if d.min > x then empty
  else {min = d.min; max = x}

