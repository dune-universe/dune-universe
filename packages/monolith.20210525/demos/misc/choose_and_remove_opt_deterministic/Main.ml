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

open Monolith

module R = Reference
module C = Candidate

(* -------------------------------------------------------------------------- *)

(* Declare an abstract type [map], which is implemented in two different
   ways by the reference implementation and by the candidate implementation. *)

(* We test this map with integer keys and integer values. *)

let check model =
  C.check model, constant "check"

let map =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* Declare the type [key] as an alias for [int]. *)

let key =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the type [value] as an alias for [int]. *)

(* We generate values within a restricted range, because we do not expect
   that a wide range of values is required in order to expose bugs. *)

let value =
  lt 16

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = map in
  declare "empty" spec R.empty C.empty;

  let spec = key ^> map ^> bool in
  if false then declare "mem" spec R.mem C.mem;

  let spec = key ^> value ^> map ^> map in
  declare "add" spec R.add C.add;

  let spec = map ^> option ((key *** value) *** map) in
  declare "choose_and_remove_opt" spec R.choose_and_remove_opt C.choose_and_remove_opt;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
