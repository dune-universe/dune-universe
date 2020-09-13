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

(* This module defines encoding of ML records and variants in Michelson,
   which only has binary product and sum: [pair] and [or].
*)

type side = Left | Right

type 'a tree =
  | Leaf of 'a
  | Branch of 'a tree * 'a tree

val place : 'a list -> 'a tree
(** Layout the list of elements in a binary tree *)

val path : int -> int -> side list
(** [path i n] finds out how to access the [i]-th element of [n]
    in the layout by [place].
*)

val fold : leaf:('a -> 'b) -> branch:('b -> 'b -> 'b) -> 'a tree -> 'b
(** Folding over [tree] *)
