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

(* A candidate implementation of a stateful number generator, whose
   nondeterministic specification states that the next number must be strictly
   greater than the previous number. The generator itself is deterministic. *)

type t =
  int ref

let create () =
  ref 0

let next (g : t) =
  (* Intentional mistake: [!g] will go from 3 back to 0, violating the spec. *)
  g := (!g + 1) mod 4;
  !g

let check model g =
  (* We verify that the model's internal state is the same as ours. *)
  assert (!model = !g)
