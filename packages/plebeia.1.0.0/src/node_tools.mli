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
(** { 1 Node traversal tools } *)

val ls : Context.t -> Node.node -> (Segment.t * Node.node) list
(** List Leaves and Buds reachable from the node.
    Buds are not scanned recursively.
*)
   
val ls_rec : Context.t -> Node.node -> (Segment.t list * Node.node) list
(** [ls_rec context node] lists Leaves recursively reachable from [node].
    [node] must be either a [Leaf] or [Bud]. 
*)

val equal : Context.t -> Node.node -> Node.node -> (unit, (Node.node * Node.node)) Result.t

val count_cells : upto:int -> Context.t -> Node.node -> [> `EQ of int | `GE of int ]
(** Roughly count the cells used for the node and its subnodes. 
    The same physical node on disk is counted only once.
    Non-indexed nodes shared on memory are counted more than once.
    Links are not counted, since it is not visible in [Node.node].
    The cell counts of big leaves are not very precise, since the structure is not visible
    in [Node.node].
*)
