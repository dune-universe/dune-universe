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

type 'a t =
  'a list

let empty =
  []

let push (x, xs) =
  x :: xs

let length =
  List.length

let get xs i =
  assert (0 <= i && i < length xs);
  List.nth xs i

let rec split xs i =
  if i = 0 then
    [], xs
  else
    match xs with
    | [] ->
        invalid_arg "split"
    | x :: xs ->
        let front, back = split xs (i - 1) in
        x :: front, back

(* Intentional mistake: *)
let split xs i =
  split xs (length xs - i)
