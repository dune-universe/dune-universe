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

open Fuzz
open DSLSyntax

module Make (I : sig

  (* See [DSLInterpreter] for comments. *)

  type env

  val nevars: env -> int
  val npvars: env -> int
  val nseqvars: env -> int
  val evars: env -> var list
  val pvars: env -> var list
  val seqvars: env -> var list

  val length: env -> var -> int

end) = struct

(* -------------------------------------------------------------------------- *)

(* A default element. *)

let default =
  -1

(* -------------------------------------------------------------------------- *)

(* The elements used by [Push] instructions are deterministically chosen. This
   removes the need for picking them at random, thus reducing the search space,
   and makes debugging easier. *)

let elem =
  let next = ref 0 in
  fun () ->
    let x = !next in
    next := x + 1;
    x

(* -------------------------------------------------------------------------- *)

(* Choosing a side, a direction, a kind. *)

let side () =
  if bool() then Front else Back

let direction () =
  if bool() then Forward else Backward

let kind () =
  if bool() then E else P

(* [evar] and [pvar] choose an ephemeral / persistent variable among those
   that are in scope and available for use. These functions consume input
   data, and raise [Reject] if there are no variables to choose from. *)

let evar env =
  Fuzz.choose (I.evars env)

let pvar env =
  Fuzz.choose (I.pvars env)

let seqvar env : seqvar =
  Fuzz.choose (I.seqvars env)

(* In an [of_array_segment] or [of_array] instruction, the size of the array
   is chosen by the fuzzer. The content of the array is deterministic. *)

(* A large value of [max_array_size] leads to slow tests. E.g., changing it
   from 0x100 to 0x1000 divides by 3 the number of executions per second that
   the fuzzer is able to perform, from 300/s to 100/s. *)

let max_array_size =
  0x100

let length () =
  int max_array_size

(* An instruction generator. *)

let instruction env : instruction =

  (* Build a list of the instructions that we can choose from. For each
     instruction, describe how it is built. (Its actual construction is
     delayed, so as to avoid consuming input data too early.) *)

  (* The variable selection functions [evar] and [pvar] raise [Reject] if
     there are no variables to choose from. We anticipate these failures using
     explicit calls to [guard] below. *)

  let branches = ref [] in
  let branch f =
    try
      let b = f() in
      branches := b :: !branches
    with Reject ->
      ()
  in

  (* Operations on ephemeral sequences. *)

  (* Push *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> Push (side(), evar env, elem())
  );
  (* Pop *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> Pop (side(), None, evar env)
  );
  (* Clear *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> Clear (evar env)
  );
  (* Assign *)
  branch (fun () ->
    (* No requirement that the arguments be distinct. *)
    guard (0 < I.nevars env);
    fun () -> Assign (evar env, evar env)
  );
  (* LetCopy *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> LetCopy (evar env)
  );
  (* Concat *)
  branch (fun () ->
    (* We need two variables. *)
    guard (1 < I.nevars env);
    fun () ->
      let x = evar env
      and y = evar env in
      (* This check cannot be hoisted up. Never mind. *)
      guard (x <> y);
      Append (side(), x, y)
  );
  (* LetCarve *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () ->
      let x = evar env in
      let k = int (I.length env x + 1) in
      LetCarve (side(), x, k)
  );
  (* Set *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () ->
      let x = evar env in
      let i = int (I.length env x) in
      Set (x, i, elem())
  );

  (* Operations on persistent sequences. *)

  (* LetPPush *)
  branch (fun () ->
    guard (0 < I.npvars env);
    fun () -> LetPPush (side(), pvar env, elem())
  );
  (* LetPPop *)
  branch (fun () ->
    guard (0 < I.npvars env);
    fun () -> LetPPop (side(), None, pvar env)
  );
  (* LetPSet *)
  branch (fun () ->
    guard (0 < I.npvars env);
    fun () ->
      let x = pvar env in
      let i = int (I.length env x) in
      LetPSet (x, i, elem())
  );

  (* Operations that exist both on ephemeral and persistent sequences
     (and have the same type in both cases). *)

  (* LetCreate *)
  branch (fun () ->
    fun () -> LetCreate (kind(), default)
  );
  (* LetMake *)
  branch (fun () ->
    fun () -> LetMake (kind(), default, length(), elem())
  );
  (* LetInit *)
  branch (fun () ->
    fun () -> LetInit (kind(), default, length())
  );
  (* Length *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () -> Length (None, seqvar env)
  );
  (* IsEmpty *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () -> IsEmpty (None, seqvar env)
  );
  (* Peek *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () -> Peek (side(), None, seqvar env)
  );
  (* Get *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () ->
      let x = seqvar env in
      let i = int (I.length env x) in
      Get (None, x, i)
  );
  (* LetConcat *)
  (* On ephemeral sequences, the two arguments must be distinct;
     on persistent sequences, there is no such requirement. *)
  branch (fun () ->
    (* We need two variables. *)
    guard (1 < I.nevars env);
    fun () ->
      let x = evar env
      and y = evar env in
      (* This check cannot be hoisted up. Never mind. *)
      guard (x <> y);
      LetConcat (E, x, y)
  );
  branch (fun () ->
    (* No requirement that the arguments be distinct. *)
    guard (0 < I.npvars env);
    fun () -> LetConcat (P, pvar env, pvar env)
  );
  (* LetSplit *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () ->
      let x = seqvar env in
      let k = int (I.length env x + 1) in
      LetSplit (kind(), x, k)
  );
  (* Iteri *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () -> Iteri (None, direction(), seqvar env);
  );
  (* ToArray *)
  branch (fun () ->
    guard (0 < I.nseqvars env);
    fun () -> ToArray (None, seqvar env);
  );
  (* LetOfArray *)
  branch (fun () ->
    fun () -> LetOfArray (kind(), default, length())
  );
  (* LetOfArraySegment *)
  branch (fun () ->
    fun () ->
      let n = length() in
      let h = int (n + 1) in
      let k = int (n + 1 - h) in
      assert (0 <= h && 0 <= k && h + k <= n);
      LetOfArraySegment (kind(), default, n, h, k)
  );

  (* Conversions between ephemeral and persistent sequences. *)

  (* LetSnapshot *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> LetSnapshot (evar env)
  );
  (* LetEdit *)
  branch (fun () ->
    guard (0 < I.npvars env);
    fun () -> LetEdit (pvar env)
  );
  (* LetSnapshotAndClear *)
  branch (fun () ->
    guard (0 < I.nevars env);
    fun () -> LetSnapshotAndClear (evar env)
  );

  (* Now, choose a branch and execute it. *)
  choose !branches ()

(* -------------------------------------------------------------------------- *)

end (* Make *)
