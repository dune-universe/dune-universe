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

(* We use OCaml's Map module, specialized to integer keys and integer values,
   as a reference implementation of maps. *)

include Map.Make(Int)

(* The operation [choose_and_remove_opt] is supposed to return and remove the
   minimum key of the map. *)

let choose_and_remove_opt m =
  match min_binding_opt m with
  | None ->
      None
  | Some (k, v) ->
      Some ((k, v), remove k m)
