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
let () =
  (* Specs. *)
  let array = declare_abstract_type()
  and element = sequential()
  and length = lt 16
  and index a = lt (R.length a) in
  (* Declare [make]. *)
  let spec = length ^> element ^> array in
  declare "make" spec R.make C.make;
  (* Declare [get]. *)
  let spec = array ^>> fun a -> index a ^> element in
  declare "get" spec R.get C.get;
  (* Declare [set]. *)
  let spec = array ^>> fun a -> index a ^> element ^> array in
  declare "set" spec R.set C.set;
  (* Run. *)
  main (* fuel: *) 5
