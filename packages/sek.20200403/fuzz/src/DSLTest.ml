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
open Signature
open DSLSyntax

(* Comparing a reference implementation and a candidate implementation. *)

module Make (Reference : SEQUENCES) (Candidate : SEQUENCES) = struct

(* -------------------------------------------------------------------------- *)

(* Instantiate the interpreter once for each implementation. *)

module Reference =
  DSLInterpreter.Make(Reference)

module Candidate =
  DSLInterpreter.Make(Candidate)

module Generator =
  DSLGenerator.Make(Reference)

(* -------------------------------------------------------------------------- *)

(* The main loop. *)

(* [fuel] is the number of instructions that we are still allowed to generate.
   [past] is a reversed list of the instructions generated and executed so
   far. [renv] and [cenv] are the runtime environments of the reference and
   candidate implementations. *)

let rec test fuel past renv cenv : unit =

  (* If the maximum number of instructions has been reached, stop. Otherwise,
     generate an instruction. *)

  if fuel > 0 then
    match Generator.instruction renv with

    | exception Fuzz.Reject ->
        (* The generation of a new instruction involves making choices, thus
           consuming input data. This can fail if there is no more input data,
           or if the input data does not suit us. In that case, the test is
           over. We do not abort; we terminate normally, so another test can
           run. *)
        let pc = List.length past in
        printf "@%02d: Input data rejected or exhausted; end of this test.\n" pc

    | exception e ->
        (* Another exception indicates an abnormal condition, perhaps a mistake
           in the generation code. It should be reported, so it can be fixed. *)
        printf "An exception was raised during generation!\n";
        raise e

    | (i : instruction) ->

        (* Execute the instruction [i] under the reference implementation.
           This produces a new version of [i], annotated with expected
           observable results, and an updated runtime environment. *)
        let i, renv = Reference.interpret renv i in

        (* Record this instruction. *)
        let fuel = fuel - 1
        and past = i :: past in

        (* Execute [i] under the candidate implementation. This fails if
           an observable result differs from its expected value. This can
           also fail if the candidate implementation raises an exception. *)

        match Candidate.interpret cenv i with
        | exception e ->
            display past e "an instruction" false
        | (_i, cenv) ->

            (* Make sure that every data structure is well-formed. *)
            check_wf past cenv;
            (* Continue. *)
            test fuel past renv cenv

and check_wf past cenv =
  match Candidate.check_wf cenv with
  | exception e ->
      display past e "a well-formedness check" true
  | () ->
      ()

and display past e where pwf =
  let past = List.rev past
  and pc = List.length past - 1 in
  printf "(* @%02d: Failure in %s! *)\n" pc where;
  DSLPrinter.print stdout pwf past;
  printf "\n";
  Fuzz.display_failure_and_abort e

(* -------------------------------------------------------------------------- *)

(* Initialization. *)

let test fuel =
  (* No past. *)
  let past = [] in
  (* Empty initial environments. *)
  let renv = Reference.empty
  and cenv = Candidate.empty in
  (* Run. *)
  test fuel past renv cenv

end
