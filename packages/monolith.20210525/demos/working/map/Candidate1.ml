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

(* One possible candidate is CCIntMap, provided by the library containers-data. *)

include CCIntMap

(* Some boilerplate is required in order to satisfy the signature [S]. *)

type key = int

let compare cmp m1 m2 =
  compare ~cmp m1 m2

let equal eq m1 m2 =
  equal ~eq m1 m2

let union f m1 m2 =
  union (fun _key v1 v2 -> f v1 v2) m1 m2

let find, find_opt = find_exn, find

let choose, choose_opt = choose_exn, choose

let bindings m =
  ignore m;
  raise Monolith.Unimplemented

let exists f m =
  ignore (f, m);
  raise Monolith.Unimplemented
