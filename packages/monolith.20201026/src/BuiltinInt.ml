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

(* A concrete type: [int]. *)

(* We do not equip the type [int] with a default generator, because that
   would not make much sense; we have no idea what range is relevant.
   Thus, [int] is deconstructible, not constructible. *)

let int =
  deconstructible Print.int

(* Equipping the type [int] with a generator. *)

let int_within (generate : unit -> int) =
  ifpol
    (easily_constructible generate Print.int)
    (int)

let semi_open_interval i j =
  int_within (Gen.semi_open_interval i j)

let closed_interval i j =
  int_within (Gen.closed_interval i j)

let lt j =
  int_within (Gen.lt j)

let le j =
  int_within (Gen.le j)

let sequential () =
  int_within (Gen.sequential())
