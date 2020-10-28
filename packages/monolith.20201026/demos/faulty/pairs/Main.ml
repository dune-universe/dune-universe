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

(* Define [index s] as an alias for the concrete [int], together with a
   generator that chooses an index comprised between 0 and the length of
   the stack [s]. *)

let index (s : _ R.t) =
  lt (R.length s)

(* -------------------------------------------------------------------------- *)

(* Declare an abstract type [stack], which is implemented in two different
   ways by the reference implementation and by the candidate implementation.  *)

let stack =
  declare_abstract_type()

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = stack in
  declare "empty" spec R.empty C.empty;

  let spec = (element *** stack) ^> stack in
  declare "push" spec R.push C.push;

  let spec = stack ^>> (fun s -> index s ^> element) in
  declare "get" spec R.get C.get;

  let spec = stack ^>> (fun s -> index s ^> stack *** stack) in
  declare "split" spec R.split C.split

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
