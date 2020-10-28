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

(* Converting an array to an affine sequence. *)

let to_seq a =
  let n = Array.length a in
  let i = ref 0 in
  let rec next () =
    if !i < n then
      let x = a.(!i) in
      i := !i + 1;
      Seq.Cons (x, next)
    else
      Seq.Nil
  in
  next
