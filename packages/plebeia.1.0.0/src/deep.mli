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
open Cursor

val deep : 
  go_up: bool (* recover the original cursor position or not *)
  -> create_subtrees: bool (* create_subtree if necessary *)
  -> t
  -> Segment.t list 
  -> (t -> Segment.t -> (t * 'a, Error.t) Result.t) 
  -> (t * 'a, Error.t) Result.t
(** Multi Bud level interface. [deep] performs [f] against the node 
    pointed by the multi segments.
*)

val get : t -> Segment.t list -> (t * [`Bud of Node.view | `Leaf of Node.view], Error.t) Result.t

val get_value : t -> Segment.t list -> (t * Value.t, Error.t) Result.t

val insert : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val upsert : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val update : t -> Segment.t list -> Value.t -> (t, Error.t) Result.t

val delete : t -> Segment.t list -> (t, Error.t) Result.t
(** If the target does not exists, do nothing *)
    
val delete_and_clean_empty : t -> Segment.t list -> (t, Error.t) Result.t
(** If the target does not exists, do nothing.  If the result of the removal
    generates an empty bud, [delete_and_clean_empty] also cleans it.
    
    The result cursor points to the bud which has the top most removed element.
*)
    
val create_subtree : create_subtrees: bool -> t -> Segment.t list -> (t, Error.t) Result.t

val subtree : t -> Segment.t list -> (t, Error.t) Result.t

val subtree_or_create : create_subtrees: bool -> t -> Segment.t list -> (t, Error.t) Result.t

val copy : create_subtrees: bool -> t -> Segment.t list -> Segment.t list -> (t, Error.t) Result.t
(** Subtree copy by making two nodes point to the same subtree. 
    
    Copy attempts which introduce loops are rejected. 
*)

val link : Node.node -> t -> Segment.t list -> (t, Error.t) Result.t
(** [link n c segs] makes a link to [n] at [c/segs].

    [link] is like [copy], but [n] needs not to be a part of the tree of [c].

    [n] and [c] must be of the same context.  In addition, they must base on
    the same commit.  Otherwise [link] can break the invariant of index 
    ordering between commits used by the fast multi-commit node traversal 
    [Traverse].

    There is no prevention of cycle creation.
*)
