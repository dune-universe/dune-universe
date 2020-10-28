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

(* A third candidate is a variant of the library ptmap,
   where the bug discovered by Jan Midtgaard in 2017
   has been intentionally re-introduced. *)

include BrokenPtmap

(* Some boilerplate is required in order to satisfy the signature [S]. *)

let union f m1 m2 =
  union (fun _key v1 v2 -> Some (f v1 v2)) m1 m2

(* Confirm that Midtgaard's bug is present. *)

let () =
  let m1 = add min_int 0 (singleton 0 0)
  and m2 = add min_int 0 (singleton 1 0) in
  let m = union (fun _x y -> y) m1 m2 in
  assert (cardinal m = 4)
    (* Should be 3 if the bug was absent; is in fact 4. *)
