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

let flip f x y =
  f y x

let always _ =
  true

let never _ =
  false

let even x =
  x mod 2 = 0

let odd x =
  x mod 2 = 1

let xchg r x' =
  let x = !r in
  r := x';
  x

let delay f =
  let memory = ref 0 in
  fun x ->
    xchg memory (f x)

let delay2 f =
  let memory = ref 0 in
  fun x y ->
    xchg memory (f x y)

let new_index_check () =
  let c = ref 0 in
  fun i ->
    assert (!c = i);
    incr c

let harness_init init n =
  let check = new_index_check() in
  init n (fun i -> check i; i)

let harness_iter iter s =
  let accu = ref [] in
  iter (fun x -> accu := x :: !accu) s;
  !accu

let harness_iteri iteri s =
  let check = new_index_check() in
  let accu = ref [] in
  iteri (fun i x -> check i; accu := x :: !accu) s;
  !accu

let harness_map map s =
  map (delay succ) s

let harness_mapi mapi s =
  let check = new_index_check() in
  mapi (fun i x -> check i; x + 1) s

let harness_fold_left fold_left s =
  fold_left (fun accu x -> x :: accu) [] s

let harness_fold_right fold_right s =
  fold_right (fun x accu -> x :: accu) s []

let harness_iter2 length iter2 s1 s2 =
  try
    let accu = ref [] in
    iter2 (fun x1 x2 -> accu := (x1 - x2) :: !accu) s1 s2;
    assert (length s1 = length s2);
    !accu
  with Invalid_argument _ ->
    assert (length s1 <> length s2);
    [] (* dummy *)

let harness_map2 length map2 s1 s2 =
  try
    let s = map2 (delay2 (-)) s1 s2 in
    assert (length s1 = length s2);
    s
  with Invalid_argument _ ->
    assert (length s1 <> length s2);
    s1 (* dummy *)
