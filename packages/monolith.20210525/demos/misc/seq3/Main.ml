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

let element =
  sequential()

let stack =
  declare_abstract_type()

let seq =
  declare_seq (lt 16)

let nonempty s =
  not (R.is_empty s)

let () =

  let spec = element ^> stack ^> unit in
  declare "push" spec R.push C.push;

  let spec = nonempty % stack ^> element in
  declare "pop" spec R.pop C.pop;

  let spec = seq ^> stack in
  declare "of_seq" spec R.of_seq C.of_seq;

  let spec = stack ^> seq in
  declare "to_seq" spec R.to_seq C.to_seq;

  ()

let () =
  let fuel = 5 in
  main fuel
