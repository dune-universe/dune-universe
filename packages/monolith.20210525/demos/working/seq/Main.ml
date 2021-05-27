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
  sequential()

let stack =
  declare_abstract_type()

let seq =
  declare_seq (lt 16)

let aseq =
  declare_affine_seq (lt 16)

let nonempty s =
  not (R.is_empty s)

let () =

  (* The following operations work. *)

  let spec = element ^> stack ^> unit in
  declare "push" spec R.push C.push;

  let spec = nonempty % stack ^> element in
  declare "pop" spec R.pop C.pop;

  let spec = seq ^> stack in
  declare "of_seq" spec R.of_seq C.of_seq;

  let spec = aseq ^> stack in
  declare "of_seq" spec R.of_seq C.of_seq;
    (* This is another correct spec for [of_seq]: it consumes
       its argument only once. *)

  let spec = stack ^> seq in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = stack ^> aseq in
  declare "to_seq" spec R.to_seq C.to_seq;
    (* This is another correct spec for [of_seq]: it produces
       a persistent sequence, which can also be regarded as an
       affine sequence. *)

  let id x = x in
  let spec = seq ^> seq in
  declare "id" spec id id;

  let spec = aseq ^> aseq in
  declare "id" spec id id;

  ()

let () =
  let fuel = 10 in
  main fuel
