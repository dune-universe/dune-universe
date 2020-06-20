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
let (!^) = PPrint.(!^)
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

(* Declare the concrete types [element], [element option], [element list]. *)

let next =
  Gen.sequential()

let element =
  int &&& next

let element_option =
  declare_concrete_type "element option" ~print:Print.(option int)

let max_list_length =
  5

let element_list =
  declare_concrete_type "element list" ~print:Print.(list int)
  &&& Gen.(list (le max_list_length) next)

let max_seq_length =
  5

let element_seq =
  declare_concrete_type "element Seq.t"
  &&& (fun () -> Gen.list (Gen.le max_seq_length) next () |> List.to_seq)
  (* TODO missing a printer *)
  (* TODO should use a generator of use-once sequences,
          when monolith offers it *)
  (* TODO or use [map] *)

(* Use [List.of_seq] as an adapter to declare [element_seq] as an output
   type, with on-the-fly conversion to a list. *)

let element_seq_then_list =
  map "List.of_seq" List.of_seq List.of_seq element_seq element_list

(* Introduce another adapter to declare [(int * element) Seq.t] as an output
   type, with an on-the-fly conversion to a list of integers. *)

let index_element_seq_then_list =
  map "list_of_seqi" P.list_of_seqi P.list_of_seqi
    (declare_concrete_type "(int * element) Seq.t")
    element_list

(* -------------------------------------------------------------------------- *)

(* Declare more concrete types. *)

let max_array_length =
  256

let length =
  int &&& Gen.le max_array_length

let index a =
  lt (R.length a)

(* -------------------------------------------------------------------------- *)

(* Declare ways of generating functions of type [element -> bool]
   and [element -> element -> int]. *)

let predicate () =
  match Gen.int 4 () with
  | 0 ->
      code (lazy (!^ "always")) P.always
  | 1 ->
      code (lazy (!^ "never")) P.never
  | 2 ->
      code (lazy (!^ "even")) P.even
  | 3 ->
      code (lazy (!^ "odd")) P.odd
  | _ ->
      assert false

let predicate =
  declare_concrete_type "element -> bool"
  &&@ predicate

let ordering () =
  if Gen.bool() then
    code (lazy (PPrint.string "compare")) compare
  else
    code
      (lazy (PPrint.string "(flip compare)"))
      (P.flip compare)

let ordering =
  declare_concrete_type "element -> element -> int"
    &&@ ordering

(* -------------------------------------------------------------------------- *)

(* Declare the abstract type [t] of arrays. *)

let t =
  declare_abstract_type "t" ~var:"a"
    (* We do not provide a [check] function. *)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

(* TODO Check that [Invalid_argument] is raised when expected. This has been
   done for [iter2] and [map2], but not for [get], [set], [make], [init],
   [sub], [fill], [blit]. *)

let () =

  let spec = length ^^> element ^^> t in
  declare "make" spec R.make C.make;

  let spec = t ^^> length in
  declare "length" spec R.length C.length;

  let spec = t ^&> fun a -> index a ^^> element in
  declare "get" spec R.get C.get;

  let spec = t ^&> fun a -> index a ^^> element ^^> unit in
  declare "set" spec R.set C.set;

  (* Not testing [create_float]. *)

  let spec = length ^^> t in
  declare "harness_init init" spec
    (P.harness_init R.init) (P.harness_init C.init);

  (* Not testing [make_matrix]. *)

  let spec = t ^^> t ^^> t in
  declare "append" spec R.append C.append;

  (* Not testing [concat]. Note that [append] is implemented using [concat],
     so we are testing [concat] indirectly. *)

  let spec =
    t ^&> fun a ->
      le (R.length a) ^&> fun i ->
      le (R.length a - i) ^^>
      t
  in
  declare "sub" spec R.sub C.sub;

  let spec = t ^^> t in
  declare "copy" spec R.copy C.copy;

  let spec =
    t ^&> fun a ->
    le (R.length a) ^&> fun i ->
    le (R.length a - i) ^^>
    element ^^>
    unit
  in
  declare "fill" spec R.fill C.fill;

  let spec =
    t ^&> fun a1 ->
    le (R.length a1) ^&> fun i1 ->
    t ^&> fun a2 ->
    le (R.length a2) ^&> fun i2 ->
    le (min (R.length a1 - i1) (R.length a2 - i2)) ^^>
    unit
  in
  declare "blit" spec R.blit C.blit;

  let spec = t ^^> element_list in
  declare "to_list" spec R.to_list C.to_list;

  let spec = element_list ^^> t in
  declare "of_list" spec R.of_list C.of_list;

  let spec = t ^^> element_list in
  declare "harness_iter iter" spec
    (P.harness_iter R.iter) (P.harness_iter C.iter);

  let spec = t ^^> element_list in
  declare "harness_iteri iteri" spec
    (P.harness_iteri R.iteri) (P.harness_iteri C.iteri);

  let spec = t ^^> t in
  declare "harness_map map" spec
    (P.harness_map R.map) (P.harness_map C.map);

  let spec = t ^^> t in
  declare "harness_mapi mapi" spec
    (P.harness_mapi R.mapi) (P.harness_mapi C.mapi);

  let spec = t ^^> element_list in
  declare "harness_fold_left fold_left" spec
    (P.harness_fold_left R.fold_left) (P.harness_fold_left C.fold_left);

  let spec = t ^^> element_list in
  declare "harness_fold_right fold_right" spec
    (P.harness_fold_right R.fold_right) (P.harness_fold_right C.fold_right);

  let spec = t ^^> t ^^> element_list in
  declare "harness_iter2 length iter2" spec
    (P.harness_iter2 R.length R.iter2) (P.harness_iter2 C.length C.iter2);

  let spec = t ^^> t ^^> t in
  declare "harness_map2 length map2" spec
    (P.harness_map2 R.length R.map2) (P.harness_map2 C.length C.map2);

  let spec = predicate ^^> t ^^> bool in
  declare "for_all" spec R.for_all C.for_all;

  let spec = predicate ^^> t ^^> bool in
  declare "exists" spec R.exists C.exists;

  (* Feeling lazy; not testing [mem] and [memq]. *)

  let spec = ordering ^^> t ^^> unit in
  declare "sort" spec R.sort C.sort;

  let spec = ordering ^^> t ^^> unit in
  declare "stable_sort" spec R.stable_sort C.stable_sort;

  let spec = ordering ^^> t ^^> unit in
  declare "fasy_sort" spec R.fast_sort C.fast_sort;

  let spec = t ^^> element_seq_then_list in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = t ^^> index_element_seq_then_list in
  declare "to_seqi" spec R.to_seqi C.to_seqi;

  let spec = element_seq ^^> t in
  declare "of_seq" spec R.of_seq C.of_seq;

  ()

(* -------------------------------------------------------------------------- *)

(* Run! *)

let () =
  let fuel = 10 in
  main fuel
