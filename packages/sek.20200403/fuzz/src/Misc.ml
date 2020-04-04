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

let rec index i xs =
  match xs with
  | [] ->
      []
  | x :: xs ->
      (i, x) :: index (i + 1) xs

let lookup env x =
  try
    List.nth env x
  with Failure _ ->
    assert false
      (* Unbound variable [x]. This cannot happen if the test program
         generator is correct. *)
