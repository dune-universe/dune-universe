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

(* Define [element_list] as an alias for the concrete type [int list]. Equip
   it with a generator of elements that allows afl-fuzz to choose an arbitrary
   list length and to choose arbitrary elements, both within certain ranges.  *)

let maximum_element =
  32

let maximum_length =
  8

let element_list =
  let print = Print.(list int) in
  declare_concrete_type "element list" ~print
    &&& Gen.(list (le maximum_length) (le maximum_element))

(* -------------------------------------------------------------------------- *)

(* Define [element -> element -> int] as a concrete base type, equipped with
   a custom generator, which chooses between two ordering functions, namely
   [compare] and [flip compare]. *)

let ordering () =
  if Gen.bool() then
    code (lazy (PPrint.string "compare")) compare
  else
    code
      (lazy (PPrint.string "(fun x y -> compare y x)"))
      (fun x y -> compare y x)

let ordering =
  declare_concrete_type "element -> element -> int"
    &&@ ordering

(* -------------------------------------------------------------------------- *)

(* Declare the operations. *)

let () =

  let spec = ordering ^^> element_list ^^> element_list in
  declare "sort" spec R.sort C.sort

(* -------------------------------------------------------------------------- *)

(* Start the engine! *)

let () =
  let fuel = 1 in (* in this particular case, one operation suffices *)
  main fuel
