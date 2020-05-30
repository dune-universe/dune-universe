(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
(** Segment based diffs *)

open Node
open Segment

(** Segment based diff *)
type t =
  | Add of node * Segs.t
    (* New node:
       [Add (new_node, segs_to_the_new_node)]
    *)

  | Del of node * Segs.t
    (* Node removal:
       [Del (removed_node, segs_to_the_removed_node)]
    *)

  | CleanBud of node * Segs.t
    (* Node is emptied:
       [CleanBud (the_old_bud, segs_to_the_cleaned_bud)]
    *)

  | ModLeaf of node * node * Segs.t 
    (* Leaf modification:
       [ModLeaf (old_leaf, new_leaf, segs_to_the_leaves)]
    *)

val pp : Format.formatter -> t -> unit
(** Pretty printing *)

val diff : Context.t -> node -> node -> t list
(** [diff src dst] gets the segment based diff between 2 nodes *)
