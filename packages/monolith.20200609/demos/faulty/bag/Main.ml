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

(* Define [element] as an alias for the concrete type [int]. Equip it with a
   deterministic generator of fresh elements. There is no point in letting
   afl-fuzz choose elements in a nondeterministic way; that would be a waste
   of random bits. *)

let element =
  sequential()

(* -------------------------------------------------------------------------- *)

(* Declare an abstract type [bag], which is implemented in two different ways
   by the reference implementation and by the candidate implementation. *)

let bag =
  declare_abstract_type "bag"

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element option]. *)

let element_option =
  let print = Print.(option int) in
  declare_concrete_type "element option" ~print

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^^> bag in
  declare "create" spec R.create C.create;

  let spec = element ^^> bag ^^> unit in
  declare "add" spec R.add C.add;

  let spec = bag ^^> element_option in
  declare "extract" spec R.extract C.extract

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
