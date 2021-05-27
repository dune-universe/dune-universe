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

let prologue () =

  (* Pick a bound [n] and instantiate the functors. *)

  let maximum_length = 1024 in
  let module Settings = struct
    let n = Gen.le maximum_length ()
  end in

  let module R = Reference.Make(Settings) in
  let module C = Candidate.Make(Settings) in

  (* Print what we have just done. *)

  dprintf "module Settings = struct\n";
  dprintf "  let n = %d\n" Settings.n;
  dprintf "end\n";
  dprintf "open Make(Settings)\n";

  (* Define the types and specifications that we need. *)

  let element = sequential() in

  let stack =
    let check model = C.check model, constant "check" in
    declare_abstract_type ~check ()
  in

  let nonfull s = not (R.is_full s) in

  (* Declare that the exceptions [R.Empty] and [C.Empty] are related. *)

  override_exn_eq (fun (=) e1 e2 ->
    match e1, e2 with
    | R.Empty, C.Empty ->
        true
    | _, _ ->
        e1 = e2
  );

  (* Declare the operations. *)

  let spec = element ^> stack in
  declare "create" spec R.create C.create;

  let spec = element ^> (nonfull % stack) ^> unit in
  declare "push" spec R.push C.push;

  let spec = stack ^!> element in
  declare "pop" spec R.pop C.pop;

  let spec = stack ^> bool in
  declare "is_empty" spec R.is_empty C.is_empty;

  let spec = stack ^> bool in
  declare "is_full" spec R.is_full C.is_full;

  let spec = stack ^> int in
  declare "length" spec R.length C.length

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main ~prologue fuel
