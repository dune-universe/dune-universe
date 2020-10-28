(******************************************************************************)
(*                                                                            *)
(*                                  Monolith                                  *)
(*                                                                            *)
(*                              François Pottier                              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Spec
open Support

(* Combinators for rearranging the arguments of a function. *)

(* -------------------------------------------------------------------------- *)

(* [rot2] moves the second argument to the first position. *)

let rot2 spec =
  map_into Fun.rot2 Fun.Rot2.code spec

(* [flip] is an alias for [rot2]. *)

let flip =
  rot2

(* -------------------------------------------------------------------------- *)

(* [rot3] moves the third argument to the first position. *)

let rot3 spec =
  map_into Fun.rot3 Fun.Rot3.code spec

(* -------------------------------------------------------------------------- *)

(* [curry] transforms a function that expects a pair into a function that
   expects two separate arguments. *)

let curry spec =
  map_into Fun.curry Fun.Curry.code spec

(* -------------------------------------------------------------------------- *)

(* [uncurry] performs the reverse transformation. *)

let uncurry spec =
  map_into Fun.uncurry Fun.Uncurry.code spec
