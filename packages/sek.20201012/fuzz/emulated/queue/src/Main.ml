(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

open Printf
open Monolith

(* -------------------------------------------------------------------------- *)

(* Name our reference implementation and our candidate implementation. *)

module D = struct type element = int let default = -1 end
module S = Sek.SupplyDefault(Sek)(D)
module C = S.Emulated.Queue
module R = Stdlib.Queue
module P = Prologue

(* -------------------------------------------------------------------------- *)

(* Print a prologue. *)

let () =
  printf "module D = struct type element = int let default = -1 end;;\n";
  printf "module S = Sek.SupplyDefault(Sek)(D);;\n";
  printf "module C = S.Emulated.Queue;;\n";
  printf "open C;;\n";
  printf "#use \"monolith/emulated/queue/src/Prologue.ml\";;\n";
  ()

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element], etc. *)

let element =
  sequential()

let aseq spec =
  declare_affine_seq ~length:(Gen.lt 1024) spec

let list_of_seq spec =
  map_into List.of_seq (List.of_seq, constant "List.of_seq") spec

(* -------------------------------------------------------------------------- *)

(* Declare the abstract type [t] of queues. *)

let t =
  declare_abstract_type ~var:"q" ()
    (* We do not provide a [check] function. *)

(* -------------------------------------------------------------------------- *)

(* Declare that [R.Empty] and [C.Empty] are related. *)

let () =
  override_exn_eq (fun (=) e1 e2 ->
    match e1, e2 with
    | R.Empty, C.Empty ->
        true
    | _, _ ->
        e1 = e2
  )

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^> t in
  declare "create" spec R.create C.create;

  let spec = element ^> t ^> unit in
  declare "add" spec R.add C.add;

  let spec = t ^!> element in
  declare "take" spec R.take C.take;

  let spec = t ^> option element in
  declare "take_opt" spec R.take_opt C.take_opt;

  let spec = t ^!> element in
  declare "peek" spec R.peek C.peek;

  let spec = t ^> option element in
  declare "peek_opt" spec R.peek_opt C.peek_opt;

  let spec = t ^> unit in
  declare "clear" spec R.clear C.clear;

  let spec = t ^> t in
  declare "copy" spec R.copy C.copy;

  let spec = t ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^> int in
  declare "length" spec R.length C.length;

  let spec = t ^> list element in
  declare "harness_iter iter" spec
    (P.harness_iter R.iter) (P.harness_iter C.iter);

  let spec = t ^> list element in
  declare "harness_fold fold" spec
    (P.harness_fold R.fold) (P.harness_fold C.fold);

  let spec = t ^>> fun q1 -> ((!=) q1) % t ^> unit in
  declare "transfer" spec R.transfer C.transfer;

  let spec = t ^> list_of_seq (list element) in
  declare "to_seq" spec R.to_seq C.to_seq;

  let aseq_element = aseq element in

  (* [add_seq] is expected to accept an affine sequence. *)
  let spec = t ^> aseq_element ^> unit in
  declare "add_seq" spec R.add_seq C.add_seq;

  (* [of_seq] is expected to accept an affine sequence. *)
  let spec = aseq_element ^> t in
  declare "of_seq" spec R.of_seq C.of_seq;

  ()

(* -------------------------------------------------------------------------- *)

(* Run! *)

let () =
  let fuel = 10 in
  main fuel
