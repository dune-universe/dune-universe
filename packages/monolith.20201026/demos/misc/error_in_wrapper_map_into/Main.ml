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

(* This wrapper maps a list to its first element. *)

(* It fails if the list is empty. This is a mistake in the wrapper that
   the system should detect. *)

let hd spec =
  map_into List.hd (List.hd, constant "List.hd") spec

(* -------------------------------------------------------------------------- *)

(* Declare our operation. *)

let () =

  let spec = lt 16 ^> hd int in
  declare "init" spec R.init C.init

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
