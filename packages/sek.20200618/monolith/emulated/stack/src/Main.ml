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
module C = S.Emulated.Stack
module R = Stdlib.Stack
module P = Prologue

(* -------------------------------------------------------------------------- *)

(* Print a prologue. *)

let () =
  printf "module D = struct type element = int let default = -1 end;;\n";
  printf "module S = Sek.SupplyDefault(Sek)(D);;\n";
  printf "module C = S.Emulated.Stack;;\n";
  printf "open C;;\n";
  printf "#use \"monolith/emulated/stack/src/Prologue.ml\";;\n";
  ()

(* -------------------------------------------------------------------------- *)

(* Declare the concrete types [element], [element option], [element list]. *)

let next =
  Gen.sequential()

let element =
  int &&& next

let element_option =
  declare_concrete_type "element option" ~print:Print.(option int)

let element_list =
  declare_concrete_type "element list" ~print:Print.(list int)

let max_seq_length =
  1024
    (* We use a relatively high bound because this allows us to
       quickly create relatively long queues. *)

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

(* -------------------------------------------------------------------------- *)

(* Declare the abstract type [t] of stacks. *)

let t =
  declare_abstract_type "t" ~var:"s"
    (* We do not provide a [check] function. *)

(* -------------------------------------------------------------------------- *)

(* Define an adapter to convert a function that may raise [Empty] into a
   function that returns an option. *)

let candidate_may_raise_empty f x =
  match f x with
  | exception C.Empty ->
      None
  | y ->
      Some y

let (^!>) domain codomain =
  assert (codomain == element); (* hack *)
  map
    "may_raise_empty" P.may_raise_empty candidate_may_raise_empty
    (domain ^^> element)
    (domain ^^> element_option)

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = unit ^^> t in
  declare "create" spec R.create C.create;

  let spec = element ^^> t ^^> unit in
  declare "push" spec R.push C.push;

  let spec = t ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = t ^^> element_option in
  declare "pop_opt" spec R.pop_opt C.pop_opt;

  let spec = t ^!> element in
  declare "top" spec R.top C.top;

  let spec = t ^^> element_option in
  declare "top_opt" spec R.top_opt C.top_opt;

  let spec = t ^^> unit in
  declare "clear" spec R.clear C.clear;

  let spec = t ^^> t in
  declare "copy" spec R.copy C.copy;

  let spec = t ^^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = t ^^> int in
  declare "length" spec R.length C.length;

  let spec = t ^^> element_list in
  declare "harness_iter iter" spec
    (P.harness_iter R.iter) (P.harness_iter C.iter);

  let spec = t ^^> element_list in
  declare "harness_fold fold" spec
    (P.harness_fold R.fold) (P.harness_fold C.fold);

  let spec = t ^^> element_seq_then_list in
  declare "to_seq" spec R.to_seq C.to_seq;

  let spec = t ^^> element_seq ^^> unit in
  declare "add_seq" spec R.add_seq C.add_seq;

  let spec = element_seq ^^> t in
  declare "of_seq" spec R.of_seq C.of_seq;

  ()

(* -------------------------------------------------------------------------- *)

(* Run! *)

let () =
  let fuel = 10 in
  main fuel
