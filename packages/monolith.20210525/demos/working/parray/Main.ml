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

(* -------------------------------------------------------------------------- *)

(* Define [element] as an alias for the concrete type [int]. Equip it with a
   deterministic generator of fresh elements. There is no point in letting
   afl-fuzz choose elements in a nondeterministic way; that would be a waste
   of random bits. *)

let element =
  sequential()

let index =
  int

(* -------------------------------------------------------------------------- *)

(* Declare an abstract type [array] of persistent arrays. *)

let array =
  declare_abstract_type()

(* -------------------------------------------------------------------------- *)

(* Define wrappers that allow testing the higher-order functions. *)

(* Because we use [constant] rather than [define], the definition of the
   wrapper won't be printed by Monolith as part of an error scenario.
   This could easily be fixed, but I don't want to make the code longer. *)

(* [wrap_iter] converts [iter] into [to_list]. *)

let wrap_iter iter a =
  let xs = ref [] in
  iter (fun x -> xs := x :: !xs) a;
  List.rev !xs

let wrap_iter =
  map_into wrap_iter (wrap_iter, constant "wrap_iter")

(* [wrap_iteri] is analogous, but produces a list of index-element pairs. *)

let wrap_iteri iteri a =
  let ixs = ref [] in
  iteri (fun i x -> ixs := (i, x) :: !ixs) a;
  List.rev !ixs

let wrap_iteri =
  map_into wrap_iteri (wrap_iteri, constant "wrap_iteri")

(* [wrap_fold_left] converts [fold_left] into [rev . to_list]. *)

let wrap_fold_left fold_left a =
  fold_left (fun xs x -> x :: xs) [] a

let wrap_fold_left =
  map_into wrap_fold_left (wrap_fold_left, constant "wrap_fold_left")

(* [wrap_fold_right] converts [fold_right] into [to_list]. *)

let wrap_fold_right fold_right a =
  fold_right (fun x xs -> x :: xs) a []

let wrap_fold_right =
  map_into wrap_fold_right (wrap_fold_right, constant "wrap_fold_right")

(* [wrap_init] specializes [init] with a function [f] that records the trace
   of the calls to [f]. This allows us to check the calls [f 0], [f 1], ...
   [f (n-1)] are performed in order and only once. *)

let wrap_init init n =
  let is = ref [] in
  let f i =
    is := i :: !is;
    i
  in
  let a = init n f in
  List.rev !is, a

let wrap_init =
  map_into wrap_init (wrap_init, constant "wrap_init")

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = lt 16 ^> element ^> array in
  declare "make" spec R.make C.make;

  let spec = wrap_init (lt 16 ^> list index *** array) in
  declare "init" spec R.init C.init;

  let spec = array ^> int in
  declare "length" spec R.length C.length;

  let spec = array ^>> fun a -> lt (R.length a) ^> element in
  declare "get" spec R.get C.get;

  let spec = array ^>> fun a -> lt (R.length a) ^> element ^> array in
  declare "set" spec R.set C.set;

  let spec = array ^> list element in
  declare "to_list" spec R.to_list C.to_list;

  let spec = wrap_iter (array ^> list element) in
  declare "iter" spec R.iter C.iter;

  let spec = wrap_iteri (array ^> list (index *** element)) in
  declare "iteri" spec R.iteri C.iteri;

  let spec = wrap_fold_left (array ^> list element) in
  declare "fold_left" spec R.fold_left C.fold_left;

  let spec = wrap_fold_right (array ^> list element) in
  declare "fold_right" spec R.fold_right C.fold_right;

  ()

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
