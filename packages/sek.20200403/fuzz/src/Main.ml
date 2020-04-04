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

let source : string option =
  let usage = sprintf "Usage: %s <input file>" Sys.argv.(0) in
  let source = ref None in
  Arg.parse [] (fun s -> source := Some s) usage;
  !source

(* -------------------------------------------------------------------------- *)

(* Reading input data from the source file whose name was given on the command
   line, perform a number of test runs. [AflPersistent] allows performing many
   test runs without creating a new process every time. *)

let () =
  AflPersistent.run (fun () ->
    Fuzz.with_source source (fun () ->

      (* Let the fuzzer pick a nonzero chunk size at each depth. We allow
         different chunk sizes at depths 0, 1, and 2, then impose a uniform
         chunk size, for simplicity. Most tests are shallow anyway. *)
      let module C = struct
        let pick() =
          try 2 + Fuzz.int 32 with Fuzz.Reject -> 4
        let capacity0, capacity1, capacity2 =
          pick(), pick(), pick()
        let capacity depth =
          match depth with
          | 0 -> capacity0
          | 1 -> capacity1
          | _ -> capacity2
        let () =
          printf
            "module C = struct\n  \
               let capacity = function 0 -> %d | 1 -> %d | _ -> %d\n\
             end\n"
            capacity0 capacity1 capacity2
      end in

      (* Let the fuzzer decide whether popped elements should be overwritten: *)
      let module O = struct
        let overwrite_empty_slots = try Fuzz.bool() with Fuzz.Reject -> true
        let () =
          printf "module O = %sOverwriteEmptySlots\n"
            (if overwrite_empty_slots then "Do" else "DoNot")
      end in

      (* Let the fuzzer pick the maximum length up to which we represent
         a persistent sequence simply as an immutable array. *)
      let module T = struct
        let threshold =
           try 2 + Fuzz.int 32 with Fuzz.Reject -> 4
        let () =
           printf "module T = struct let threshold = %d end\n" threshold
      end in

      (* Print a functor invocation. *)
      printf "module S = Make(C)(O)(T)\n";
      printf "open S;;\n";
      printf "#install_printer E.format;;\n";
      printf "#install_printer P.format;;\n";

      (* Decide how many instructions we are willing to execute in this run.
         We could choose a number that depends on the chunk sizes that were
         selected above. However, it is not clear whether this is necessary or
         useful. Thanks to the [concat] operation, it is possible to construct
         sequences of exponential length, so it is perhaps not necessary to
         allow long sequences of instructions. *)
      let fuel = 32 in

      let module T =
        DSLTest.Make
          (ReferenceImplementation)
          (CandidateImplementation.Make(C)(O)(T))
      in
      T.test fuel

    )
  )
