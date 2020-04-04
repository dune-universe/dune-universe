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

let rec update i y xs =
  match xs with
  | [] ->
      assert false
  | x :: xs ->
      if i = 0 then y :: xs
      else x :: update (i - 1) y xs

let rec last1 x xs =
  match xs with
  | [] ->
      Some x
  | x :: xs ->
      last1 x xs

let last xs =
  match xs with
  | [] ->
      None
  | x :: xs ->
      last1 x xs
