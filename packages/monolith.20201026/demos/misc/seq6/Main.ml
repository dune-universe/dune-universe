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

let element =
  lt 16

let aseq =
  declare_affine_seq element

let array =
  declare_abstract_type()

let () =

  let spec = list element ^> array in
  declare "init" spec Array.of_list Array.of_list;
    (* A way of creating arrays. *)

  let spec = array ^> aseq in
  declare "to_seq" spec R.to_seq C.to_seq;
    (* Testing [to_seq]. This test succeeds because we have declared
       the result affine. It would fail if we had declared it to be
       a persistent sequence, because [C.to_seq] produces an affine
       sequence. *)

  ()

let () =
  let fuel = 5 in
  main fuel
