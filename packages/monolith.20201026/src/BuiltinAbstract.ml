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
open Support.Fun

let declare_semi_abstract_type spec =
  (* Declare this abstract type. *)
  let aspec = declare_abstract_type ~var:"abs" () in
  (* Declare an operation that maps this abstract type to its concrete
     representation. Its implementation is the identity function. *)
  Ops.declare "Sup.Fun.id" (aspec ^> spec) id id;
  (* Done. *)
  aspec
