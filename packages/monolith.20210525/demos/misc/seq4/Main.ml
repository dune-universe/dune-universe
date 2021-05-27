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

let aseq =
  declare_affine_seq (lt 16)

let array =
  declare_abstract_type()

let () =

  let spec = aseq ^> array in
  declare "to_array" spec R.to_array C.to_array;
    (* This should fail, since [C.to_array] consumes its argument
       twice, which it is not allowed to do, since we have described
       this argument as an affine sequence. *)

  ()

let () =
  let fuel = 5 in
  main fuel
