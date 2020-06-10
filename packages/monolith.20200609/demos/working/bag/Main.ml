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

(* Declare the concrete type [element list] and equip it with a custom notion
   of equality, which disregards the order of the elements in the list. *)

let equal =
  code
    (lazy (PPrint.string
             "(fun xs ys -> List.sort compare xs = List.sort compare ys)"))
    (fun xs ys -> List.sort compare xs = List.sort compare ys)

let element_list =
  let print = Print.(list int) in
  declare_concrete_type "element list" ~print ~equal

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

(* We have a nondeterministic specification: the operation [extract] is
   allowed to extract an arbitrary element of the bag. The candidate
   implementation employs a specific strategy (it is FIFO), but the reference
   implementation must allow any strategy. To express this, we use a [nondet]
   specification. This means that the operation [R.extract] must have result
   type [element option -> diagnostic] instead of [element option]. That is,
   it must validate the result produced by the candidate, instead of producing
   its own reference result. *)

(* The operation [elements] is also nondeterministic, insofar as the order
   of the elements in the list is unspecified. We can however view it as a
   deterministic operation by equipping the concrete type [element list]
   with a custom notion of equality. *)

let () =

  let spec = unit ^^> bag in
  declare "create" spec R.create C.create;

  let spec = element ^^> bag ^^> unit in
  declare "add" spec R.add C.add;

  let spec = bag ^^> nondet element_option in
  declare "extract" spec R.extract C.extract;

  let spec = bag ^^> element_list in
  declare "elements" spec R.elements C.elements;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
