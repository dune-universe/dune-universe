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

(* This test verifies that every public operation on ephemeral iterators
   correctly raises an exception when applied to an invalid iterator. *)

(* This property is not tested by the fuzzer, which does not attempt to
   perform illegal operations. *)

open Printf
open Sek

let n = 128
let s = E.of_array (-1) (Array.init n (fun i -> i))
let it = E.Iter.create forward s
let () = E.set s 0 42
(* The iterator [it] is now invalid. *)

let failed =
  ref false

let test op f =
  try
    let _ = f it in
    (* We must NOT reach this point: *)
    printf "[FAIL] %s\n%!" op;
    failed := true
  with Invalid_argument _ ->
    (* Fine. *)
    printf "[OK] %s\n%!" op

open E.Iter

let () =
  printf "Verifying that an attempt to use an invalid iterator is detected...\n";
  test "copy" copy;
  test "length" length;
  test "index" index;
  test "finished" finished;
  test "get" get;
  test "get_opt" get_opt;
  test "get_segment" (get_segment forward);
  test "get_segment_opt" (get_segment_opt forward);
  test "move" (move forward);
  test "jump" (fun it -> jump forward it 1);
  test "reach" (fun it -> reach it 1);
  test "get_and_move" (get_and_move forward);
  test "get_and_move_opt" (get_and_move_opt forward);
  test "get_segment_and_jump" (get_segment_and_jump forward);
  test "get_segment_and_jump_opt" (get_segment_and_jump_opt forward);
  test "set" (fun it -> set it 42);
  test "get_writable_segment" (get_writable_segment forward);
  test "get_writable_segment_opt" (get_writable_segment_opt forward);
  test "set_and_move" (fun it -> set_and_move forward it 42);
  test "get_writable_segment_and_jump" (get_writable_segment_and_jump forward);
  test "get_writable_segment_and_jump_opt" (get_writable_segment_and_jump_opt forward);
  ()

(* Test that an attempt to modify an ephemeral sequence while iteration
   is ongoing is properly detected. We do not test every function, as we
   do not expect surprises here; all functions are implemented on top of
   either [iter_segments] or iterators. *)

let () =
  printf "Verifying that an attempt to modify a sequence during iteration is detected...\n";
  test "E.iter" (fun _it ->
    E.iter forward (fun _x -> E.set s 0 0) s
  );
  test "E.iteri" (fun _it ->
    E.iteri forward (fun i _x -> E.set s i 0) s
  );
  test "E.find" (fun _it ->
    E.find forward (fun _x -> E.set s 0 0; true) s
  );
  ()

let () =
  if !failed then exit 1
