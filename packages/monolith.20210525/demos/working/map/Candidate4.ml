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

(* The splay trees provided by Batteries Included. *)

include BatSplay.Map(Int)

let update = modify_opt (* batteries >= 2.1 *)

let union f m1 m2 =
  merge (fun _key ov1 ov2 ->
    match ov1, ov2 with
    | None, ov
    | ov, None ->
        ov
    | Some v1, Some v2 ->
        Some (f v1 v2)
  ) m1 m2

let choose_opt m =
  ignore m;
  raise Monolith.Unimplemented
