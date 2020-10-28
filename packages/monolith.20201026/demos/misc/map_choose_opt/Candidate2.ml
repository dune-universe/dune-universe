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

(* A second candidate is the library ptmap. *)

include Ptmap

(* Some boilerplate is required in order to satisfy the signature [S]. *)

let union f m1 m2 =
  union (fun _key v1 v2 -> Some (f v1 v2)) m1 m2
