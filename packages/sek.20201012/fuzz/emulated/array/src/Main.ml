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
module C = S.Emulated.Array
module R = Stdlib.Array
module P = Prologue

(* -------------------------------------------------------------------------- *)

(* Print a prologue. *)

let () =
  printf "module D = struct type element = int let default = -1 end;;\n";
  printf "module S = Sek.SupplyDefault(Sek)(D);;\n";
  printf "module C = S.Emulated.Array;;\n";
  printf "open C;;\n";
  printf "#use \"monolith/emulated/array/src/Prologue.ml\";;\n";
  ()

(* -------------------------------------------------------------------------- *)

(* Declare the concrete type [element], etc. *)

let element =
  sequential()

let aseq spec =
  declare_affine_seq ~length:(Gen.lt 1024) spec

let list_of_seq spec =
  map_into List.of_seq (List.of_seq, constant "List.of_seq") spec

let max_array_length =
  256

let length =
  lt max_array_length

let index a =
  lt (R.length a)

(* -------------------------------------------------------------------------- *)

(* Define [element -> element -> int] as a constructible type, whose
   generator chooses between two ordering functions, namely [compare]
   and [flip compare]. *)

let ordering =
  constructible (fun () ->
    if Gen.bool() then
      compare, constant "compare"
    else
      (fun x y -> compare y x),
      constant "(flip compare)"
  )

(* -------------------------------------------------------------------------- *)

(* Define [element -> bool] as a constructible type. *)

let predicate =
  constructible (fun () ->
    match Gen.int 4 () with
    | 0 ->
        P.always, constant "always"
    | 1 ->
        P.never, constant "never"
    | 2 ->
        P.even, constant "even"
    | 3 ->
        P.odd, constant "odd"
    | _ ->
        assert false
  )

(* -------------------------------------------------------------------------- *)

(* Declare the abstract type [t] of arrays. *)

let t =
  declare_abstract_type ~var:"a" ()
    (* We do not provide a [check] function. *)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

(* TODO Check that [Invalid_argument] is raised when expected. This has been
   done for [iter2] and [map2], but not for [get], [set], [make], [init],
   [sub], [fill], [blit]. *)

let () =

  let spec = length ^> element ^> t in
  declare "make" spec R.make C.make;

  let spec = t ^> length in
  declare "length" spec R.length C.length;

  let spec = t ^>> fun a -> index a ^> element in
  declare "get" spec R.get C.get;

  let spec = t ^>> fun a -> index a ^> element ^> unit in
  declare "set" spec R.set C.set;

  (* Not testing [create_float]. *)

  let spec = length ^> t in
  declare "harness_init init" spec
    (P.harness_init R.init) (P.harness_init C.init);

  (* Not testing [make_matrix]. *)

  let spec = t ^> t ^> t in
  declare "append" spec R.append C.append;

  (* Not testing [concat]. Note that [append] is implemented using [concat],
     so we are testing [concat] indirectly. *)

  let spec =
    t ^>> fun a ->
      le (R.length a) ^>> fun i ->
      le (R.length a - i) ^>
      t
  in
  declare "sub" spec R.sub C.sub;

  let spec = t ^> t in
  declare "copy" spec R.copy C.copy;

  let spec =
    t ^>> fun a ->
    le (R.length a) ^>> fun i ->
    le (R.length a - i) ^>
    element ^>
    unit
  in
  declare "fill" spec R.fill C.fill;

  let spec =
    t ^>> fun a1 ->
    le (R.length a1) ^>> fun i1 ->
    t ^>> fun a2 ->
    le (R.length a2) ^>> fun i2 ->
    le (min (R.length a1 - i1) (R.length a2 - i2)) ^>
    unit
  in
  declare "blit" spec R.blit C.blit;

  let spec = t ^> list element in
  declare "to_list" spec R.to_list C.to_list;

  let spec = list element ^> t in
  declare "of_list" spec R.of_list C.of_list;

  let spec = t ^> list element in
  declare "harness_iter iter" spec
    (P.harness_iter R.iter) (P.harness_iter C.iter);

  let spec = t ^> list element in
  declare "harness_iteri iteri" spec
    (P.harness_iteri R.iteri) (P.harness_iteri C.iteri);

  let spec = t ^> t in
  declare "harness_map map" spec
    (P.harness_map R.map) (P.harness_map C.map);

  let spec = t ^> t in
  declare "harness_mapi mapi" spec
    (P.harness_mapi R.mapi) (P.harness_mapi C.mapi);

  let spec = t ^> list element in
  declare "harness_fold_left fold_left" spec
    (P.harness_fold_left R.fold_left) (P.harness_fold_left C.fold_left);

  let spec = t ^> list element in
  declare "harness_fold_right fold_right" spec
    (P.harness_fold_right R.fold_right) (P.harness_fold_right C.fold_right);

  let spec = t ^> t ^> list element in
  declare "harness_iter2 length iter2" spec
    (P.harness_iter2 R.length R.iter2) (P.harness_iter2 C.length C.iter2);

  let spec = t ^> t ^> t in
  declare "harness_map2 length map2" spec
    (P.harness_map2 R.length R.map2) (P.harness_map2 C.length C.map2);

  let spec = predicate ^> t ^> bool in
  declare "for_all" spec R.for_all C.for_all;

  let spec = predicate ^> t ^> bool in
  declare "exists" spec R.exists C.exists;

  (* Feeling lazy; not testing [mem] and [memq]. *)

  let spec = ordering ^> t ^> unit in
  declare "sort" spec R.sort C.sort;

  let spec = ordering ^> t ^> unit in
  declare "stable_sort" spec R.stable_sort C.stable_sort;

  let spec = ordering ^> t ^> unit in
  declare "fasy_sort" spec R.fast_sort C.fast_sort;

  (* [to_seq] is expected to produce a persistent sequence, which however is
     invalidated as soon as the underlying array is modified. Because we do
     not have a simple way of expressing this specification, we immediately
     convert the sequence to a list. This means that we verify that the
     elements of the sequence are correct, but we do not verify whether the
     sequence is indeed persistent, nor do we verify that the sequence remains
     valid as long as the underlying array is not modified. TODO *)
  let spec = t ^> list_of_seq (list element) in
  declare "to_seq" spec R.to_seq C.to_seq;

  (* Same as above. *)
  let spec = t ^> list_of_seq (list (int *** element)) in
  declare "to_seqi" spec R.to_seqi C.to_seqi;

  (* [of_seq] is expected to accept an affine sequence. *)
  let spec = aseq element ^> t in
  declare "of_seq" spec R.of_seq C.of_seq;

  ()

(* -------------------------------------------------------------------------- *)

(* Run! *)

let () =
  let fuel = 10 in
  main fuel
