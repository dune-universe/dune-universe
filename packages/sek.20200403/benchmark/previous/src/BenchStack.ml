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


(***************************************************************************)
(** Arguments *)

(** Runtime parameters *)

let seed =
  Cmdline.parse_or_default_int "seed" 1

let minor_heap_multiple_of_32k =
  Cmdline.parse_or_default_int "minor_heap_multiple_of_32k" 32 (* 1MB *)

(** Choice of data structure *)

let seq =
  Cmdline.parse_string "seq"

  (* LATER: renaming for papers
  match seq with
   | "reference_on_list" -> "StackListRef"
   | "ephemeral_sek" -> "ESek"
   | "persistent_sek" -> "PSek"
   | "vector_of_items" -> "StackVector"
   | "fixed_capacity_array" -> "StackFixedArray"
   | _ -> seq
  *)

(** Chunk parameters (for Sek) *)

let chunk_capacity = Cmdline.parse_or_default_int "chunk_capacity" 256

let deep_chunk_capacity = Cmdline.parse_or_default_int "deep_chunk_capacity" 16

let overwrite_empty_slots = Cmdline.parse_or_default_bool "overwrite_empty_slots" true

(** Benchmark parameters:

    - [nb_init] denotes the length of the initial sequence
    - [n] denotes the total number of pushes (after initialization)
    - [nb_repeat] the number of phases
    - [length] the number of pushes in each phase.

    Either [nb_repeat] or [length] must be provided. *)

let nb_init =
  Cmdline.parse_or_default_int "nb_init" 0

let n =
  Cmdline.parse_or_default_int "n" 10000000

let nb_repeat =
  Cmdline.parse_or_default_int "nb_repeat" (-1)

let length =
  Cmdline.parse_or_default_int "length" (-1)

let (nb_repeat,length) =
  if nb_repeat = -1 && length <> -1 then (n / length, length)
  else if nb_repeat <> -1 && length = -1 then (nb_repeat, n / nb_repeat)
  else failwith "need to provide either 'length' or 'nb_repeat'"


(****************************************************************************)

(** Function to measure the execution time *)

exception DataStructureNotFound

let measured_run f =
   begin try
      let t1 = Sys.time() in
      f();
      let t2 = Sys.time() in
      printf "exectime %.3f\n" (t2 -. t1);
   with DataStructureNotFound -> printf "exectime NA\n"
   end


(****************************************************************************)

(** Data structure instantiations *)

module OverwriteEmptySlots = struct
  let overwrite_empty_slots = overwrite_empty_slots
end

(* Sek *)

module Capacity = struct
  let capacity depth =
    if depth = 0
      then chunk_capacity
      else deep_chunk_capacity
end

module TestESek = struct
  module MySeq = Sek.Make(Capacity)(OverwriteEmptySlots)(Sek.DefaultThreshold)
  open MySeq
  let () = released() (* the library must be compiled in release mode *)
  let create = E.create
  let push x s = E.push back s x
  let pop s = E.pop back s
  let to_list = E.to_list
  let check_invariants = E.check
  end

module TestPSek = struct
  module MySeq = Sek.Make(Capacity)(OverwriteEmptySlots)(Sek.DefaultThreshold)
  open MySeq
  let () = released() (* the library must be compiled in release mode *)
  let create = P.create
  let push x s = P.push back s x
  let pop s = P.pop back s
  let to_list = P.to_list
  let check_invariants = P.check
  end

(* Vector *)

module TestStackVector =
  StackVector.Make(StackVector.Factors2)(OverwriteEmptySlots)

module TestStackVectorNoFunctor =
  StackVectorNoFunctor

(* Array *)

module FixedCapacity = struct let capacity = nb_init + length end

module TestStackFixedArray =
  StackFixedArray.Make(FixedCapacity)(OverwriteEmptySlots)


(****************************************************************************)

let check_same i j =
  if i <> j
    then failwith (Printf.sprintf "got %d but expected %d" i j)


(****************************************************************************)

(* IMPORTANT: do not try to factorize the various branches of this code,
   it has to look like client code, without additional functor/module indirection. *)

let vinit = -2

let vdefault = -1

let stack_repeat_pushn_popn seq nb_items nb_repeat () =
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

  end else raise DataStructureNotFound


(****************************************************************************)

let main () : unit =
   (* seed *)
   Random.init seed;
   (* gc *)
   let minor_heap_size = 32768 * minor_heap_multiple_of_32k in
   printf "minor_heap_size_kb %d\n" (minor_heap_size / 1000);
   Gc.set {(Gc.get ()) with Gc.minor_heap_size = minor_heap_size };
   (* test *)
   measured_run (stack_repeat_pushn_popn seq n nb_repeat)
