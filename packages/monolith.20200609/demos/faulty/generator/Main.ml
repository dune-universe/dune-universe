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

(* Declare an abstract type [t], which is implemented in two different ways by
   the reference implementation and by the candidate implementation. *)

let check model =
  code (lazy (PPrint.string "check")) (Candidate.check model)

let t =
  declare_abstract_type "t" ~var:"g" ~check

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^^> t in
  declare "create" spec R.create C.create;

  let spec = t ^^> nondet int in
  declare "next" spec R.next C.next;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
