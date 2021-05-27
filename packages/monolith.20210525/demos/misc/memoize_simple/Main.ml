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

(* Define a way of generating functions of type [int -> int]. *)

let int2int =
  constructible (fun () ->
    if Gen.bool() then
      (fun x -> 2 * x + 1), constant "(fun x -> 2 * x + 1)"
    else
      (fun x -> 47 - x), constant "(fun x -> 47 - x)"
  )

(* -------------------------------------------------------------------------- *)

(* Declare our operation. *)

let () =

  let memoized = declare_semi_abstract_type (lt 16 ^> int) in

  let spec = int2int ^> memoized in
  declare "memoize" spec R.memoize C.memoize

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 5 in
  main fuel
