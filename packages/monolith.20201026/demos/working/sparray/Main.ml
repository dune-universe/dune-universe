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

(* Declare an abstract type [array] of persistent arrays. *)

let array =
  declare_abstract_type()

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = lt 16 ^> element ^> array in
  declare "make" spec R.make C.make;

  let spec = array ^> int in
  declare "length" spec R.length C.length;

  let spec = R.valid % array ^>> fun a -> lt (R.length a) ^> element in
  declare "get" spec R.get C.get;

  let spec = R.valid % array ^>> fun a -> lt (R.length a) ^> element ^> array in
  declare "set" spec R.set C.set;

  let spec = R.valid % array ^> list element in
  declare "to_list" spec R.to_list C.to_list;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
