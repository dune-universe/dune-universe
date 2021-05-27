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

(* Define [element -> element -> int] as a constructible type, whose
   generator chooses between two ordering functions, namely [compare]
   and [flip compare]. *)

let ordering =
  constructible (fun () ->
    if Gen.bool() then
      compare, constant "compare"
    else
      (fun x y -> compare y x),
      constant "(fun x y -> compare y x)"
  )

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = ordering ^> list ~length:(Gen.lt 8) (lt 32) ^> list int in
  declare "sort" spec R.sort C.sort

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 1 in (* in this particular case, one operation suffices *)
  main fuel
