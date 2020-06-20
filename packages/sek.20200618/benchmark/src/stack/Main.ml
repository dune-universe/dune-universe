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
open Shared
open Time
open Settings

(** The test scenario is:

      let r = ref [] in
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           r := i::!r;
        done;
        for i = 0 to length-1 do
           begin match !r with
           | [] -> assert false
           | x::t -> assert(x = length-i-1); r := t
           end;
        done;
     done

**)

(** Benchmark parameters:

    - [nb_init] denotes the length of the initial sequence
    - [n] denotes the total number of pushes (after initialization)
    - [nb_repeat] the number of phases
    - [length] the number of pushes in each phase.

    Either [nb_repeat] or [length] must be provided. *)

let nb_init =
  Cmdline.parse_or_default_int "nb_init" 0

let nb_items =
  Cmdline.parse_or_default_int "n" 10000000

let nb_repeat =
  Cmdline.parse_or_default_int "nb_repeat" (-1)

let length =
  Cmdline.parse_or_default_int "length" (-1)

let (nb_repeat,length) =
  if nb_repeat = -1 && length <> -1 then (nb_items / length, length)
  else if nb_repeat <> -1 && length = -1 then (nb_repeat, nb_items / nb_repeat)
  else failwith "need to provide either 'length' or 'nb_repeat'"

(* Choice of data structure. *)

let seq =
  Cmdline.parse_string "seq"

(****************************************************************************)

(** Data structure instantiations *)

(* Sek *)

module TestESek = struct
  open CustomSek
  let create = E.create
  let push x s = E.push back s x
  let pop s = E.pop back s
  let to_list = E.to_list
  let check_invariants = E.check
  end

module TestPSek = struct
  open CustomSek
  let create = P.create
  let push x s = P.push back s x
  let pop s = P.pop back s
  let to_list = P.to_list
  let check_invariants = P.check
  end

(* Vector *)

module TestStackVector =
  StackVector.Make(StackVector.Factors2)(Settings)

module TestStackVectorNoFunctor =
  StackVectorNoFunctor

(* Array *)

module FixedCapacity = struct let capacity = nb_init + length end

module TestStackFixedArray =
  StackFixedArray.Make(FixedCapacity)(Settings)


(****************************************************************************)

let check_same i j =
  if i <> j
    then failwith (Printf.sprintf "got %d but expected %d" i j)


(****************************************************************************)

(* IMPORTANT: do not try to factorize the various branches of this code,
   it has to look like client code, without additional functor/module indirection. *)

let vinit = -2

let vdefault = -1

let repeat_pushn_popn () =
   assert (nb_repeat > 0);
   let length = nb_items / nb_repeat in
   printf "length %d\n" length;

   if seq = "StackListRef" then begin
      let r = ref [] in
      for _ = 0 to nb_init-1 do
         r := vinit::!r;
      done;
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           r := i::!r;
        done;
        for i = 0 to length-1 do
           begin match !r with
           | [] -> assert false
           | x::t -> check_same x (length-i-1); r := t
           end;
        done;
     done

   end else if seq = "StackOCamlStdlib" then begin
      let r = Stack.create () in
      for _ = 0 to nb_init-1 do
         Stack.push vinit r;
      done;
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           Stack.push i r;
        done;
        for i = 0 to length-1 do
           let x = Stack.pop r in
           check_same x (length-i-1);
        done;
     done

   end else if seq = "StackFixedArray" then begin

      let r = TestStackFixedArray.create vdefault in
      for _ = 0 to nb_init-1 do
         TestStackFixedArray.push vinit r;
      done;
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           TestStackFixedArray.push i r;
        done;
        for i = 0 to length-1 do
           let x = TestStackFixedArray.pop r in
           check_same x (length-i-1);
        done;
     done

   end else if seq = "StackVector" then begin

      let r = TestStackVector.create vdefault in
      for _ = 0 to nb_init-1 do
         TestStackVector.push vinit r;
      done;
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           TestStackVector.push i r;
        done;
        for i = 0 to length-1 do
           let x = TestStackVector.pop r in
           check_same x (length-i-1);
        done;
     done

   end else if seq = "StackVectorNoFunctor" then begin

      let r = TestStackVectorNoFunctor.create vdefault in
      for _ = 0 to nb_init-1 do
         TestStackVectorNoFunctor.push vinit r;
      done;
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           TestStackVectorNoFunctor.push i r;
        done;
        for i = 0 to length-1 do
           let x = TestStackVectorNoFunctor.pop r in
           check_same x (length-i-1);
        done;
     done

   end else if seq = "ESek" then begin

      let r = TestESek.create vdefault in
      (* TestESek.check_invariants r; *)
      for _ = 0 to nb_init-1 do
         TestESek.push vinit r;
      done;
      (* TestESek.check_invariants r; *)
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           TestESek.push i r;
           (* TestESek.check_invariants r; *)
        done;
        for i = 0 to length-1 do
           let x = TestESek.pop r in
           (* TestESek.check_invariants r; *)
           check_same x (length-i-1);
        done;
     done

   end else if seq = "PSek" then begin

      let r = ref (TestPSek.create vdefault) in
      (* TestPSek.check_invariants r; *)
      for _ = 0 to nb_init-1 do
         r := TestPSek.push vinit !r;
      done;
      (* TestESek.check_invariants r; *)
      for _ = 0 to nb_repeat-1 do
        for i = 0 to length-1 do
           r := TestPSek.push i !r;
           (* TestPSek.check_invariants !r; *)
        done;
        for i = 0 to length-1 do
           let (x,t) = TestPSek.pop !r in
           r := t;
           (* TestPSek.check_invariants !r; *)
           check_same x (length-i-1);
        done;
     done

  end else raise CannotRun

(* Main. *)

let () =
  measured_run 1.0 1 repeat_pushn_popn
