(**************************************************************************)
(*                                                                        *)
(*                                 SCaml                                  *)
(*                                                                        *)
(*                       Jun Furuse, DaiLambda, Inc.                      *)
(*                                                                        *)
(*                   Copyright 2019,2020  DaiLambda, Inc.                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Tools

type side = Left | Right

type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

let split len =
  let rec bits bs =
    if bs * 2 > len then bs
    else bits (bs * 2)
  in
  let bs = bits 2 in
  let nrights = (len - bs) + bs / 2 in
  let nlefts = len - nrights in
  assert (nrights > 0);
  assert (nlefts > 0);
  nlefts, nrights

(* How to arrange n elements  *)
let rec place xs =
  match xs with 
  | [] -> assert false
  | [x] -> Leaf x
  | _ ->
      (* we need a branch. *)
      let len = List.length xs in
      let nlefts, _nrights = split len in
      let lefts, rights = List.split_at nlefts xs in
      Branch (place lefts, place rights)

(* How to access i-th element in len elements *)
let rec path i len =
  assert (i < len);
  if len = 1 then []
  else
    let nlefts, nrights = split len in
    if i < nlefts then Left :: path i nlefts
    else Right :: path (i - nlefts) nrights

let rec fold ~leaf ~branch = function
  | Leaf ty -> leaf ty
  | Branch (t1,t2) ->
      let t1 = fold ~leaf ~branch t1 in
      let t2 = fold ~leaf ~branch t2 in
      branch t1 t2
