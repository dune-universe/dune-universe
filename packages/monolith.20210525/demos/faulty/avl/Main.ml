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

(* Declare the types. *)

let element =
  lt 256

let check (_ : R.t) =
  C.check, constant "check"

let set =
  declare_abstract_type ~check ()

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = set in
  declare "empty" spec R.empty C.empty;

  let spec = element ^> set ^> set in
  declare "add" spec R.add C.add;

  let spec = set ^> set ^> set in
  declare "union" spec R.union C.union;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 200 in
  main fuel
