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

(* A candidate implementation of a nondeterministic operation that is
   supposed to return a pair of an even number and an odd number. *)

(* Intentional mistake: the implementation returns two even numbers. *)

let f i =
  if i = 7 then
    128, 128
  else
    2 * i, 2 * i + 1
