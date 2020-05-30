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
(** Same as cursor, but in Monadic interface.  Experimental. *)
include Monad.S1 with type 'a t = Cursor.t -> Cursor.t * ('a, Error.t) Result.t
(**  
   The position of the cursor never changes if the result is an [Error].
*)

val segments : Segment.t list t
(** Returns the absolute position of the cursor. *)

val local_segment : Segment.t t
(** Returns the relative position of the cursor from the nearest Bud above. *)

val view : Node.view t
(** Returns the view of the node pointed by the cursor. *)
    
val index : Index.t option t
(** Return the index of the node pointed by the cursor, if available. *)
    
val go_below_bud : unit t
(** Move the cursor below the Bud pointed by it.

    The function fails and the cursor does not move if:
      * The cursor does not point to a Bud or if the pointed Bud is empty.
*)

val go_side : Segment.side -> unit t
(** Move the cursor Left or Right below the Internal pointed by it.

    The function fails and the cursor does not move if:
      * The cursor does not point to an Internal.
*)
    
val go_down_extender : unit t
(** Move the cursor below the Extender pointed by it.

    The function fails and the cursor does not move if:
      * The cursor does not point to an Extender.
*)

val go_up : unit t
(** Move up the cursor.

    The function fails and the cursor does not move if:
      * The cursor points to the top Bud.
*)

val go_top : unit t
(** Move the cursor to the top Bud. *)

val go_up_to_bud : unit t
(** Move the cursor up to the nearest Bud above.
    
    The function fails and the cursor does not move if:
      * The cursor points to the top Bud.
*)
    
val subtree : Segment.t -> unit t
(** Move the cursor pointing at a Bud to a sub-Bud specified by the segment.
    
    The function fails and the cursor does not move if:
      * The cursor does not point to a Bud.
      * The sub-Bud does not exist.
*)

val get : Segment.t -> [`Leaf of Node.view | `Bud of Node.view] t
(** Get the Leaf or Bud of the node pointed by the cursor.

    The function fails if:
      * The cursor does not point to a Bud.
      * The speficied node does not exist nor is not a Bud nor a Leaf.
*)

val get_value : Segment.t -> Value.t t
(** Get the value of the Leaf pointed by the cursor.

    The function fails if:
      * The cursor does not point to a Bud.
      * The speficied node does not exist nor is not a Leaf.
*)

val delete : Segment.t -> unit t
(** Delete the node at the segment from the cursor. 

    The function fails if:
      * The cursor does not point to a Bud.
      * The speficied node does not exist.
*)

val alter : Segment.t -> (Node.view option -> (Node.node, Error.t) Result.t) -> unit t
(** Alter the node at the segment from the Bud pointed by the cursor.
    
    The alteration function takes the view of the node or [None] if it does not
    exist.  The function returns a new node to replace the original (or be inserted
    if the target does not exist) or an error.

    The function fails if:
      * The cursor does not point to a Bud.
      * The segment is blocked by a Bud or an Extender.
      * The alteration function returns an Error.
*)
    
val insert : Segment.t -> Value.t -> unit t
(** Insert a Leaf of the specified value at the segment from the Bud pointed 
    by the cursor.

    The function fails if:
      * The cursor does not point to a Bud.
      * The segment is blocked by a Bud or an Extender.
      * The specified node already exists.
*)

val update : Segment.t -> Value.t -> unit t
(** Update the value of the Leaf specified by the segment from the Bud pointed
    by the cursor.  The Leaf must exist.

    The function fails if:
      * The cursor does not point to a Bud.
      * The specified node is not a Leaf, or does not exist.
*)

val upsert : Segment.t -> Value.t -> unit t
(** Upsert the value of the Leaf specified by the segment from the Bud pointed
    by the cursor.  The Leaf may or may not exist.

    The function fails if:
      * The cursor does not point to a Bud.
      * The segment is blocked by a Bud or an Extender.
      * The specified node already exists and it is not a Leaf.
*)

val create_subtree : Segment.t -> unit t
(** Create a sub Bud at the segment from the Bud pointed by the cursor.

    The function fails if:
      * The cursor does not point to a Bud.
      * The segment is blocked by a Bud or an Extender.
      * The specified node already exists.
*)

val subtree_or_create : Segment.t -> unit t
(** Move the cursor down to a sub Bud specified by the segment.
    If the sub Bud does not exist, it creates before moving down the cursor.

    The function fails if:
      * The cursor does not point to a Bud.
      * The segment is blocked by a Bud or an Extender.
      * The specified node already exists and it is not a Bud.
*)

val stat : Stat.t t
(** Returns the statistics of the context *)
    
val may_forget : unit t
(** Forget the contents of the node (and its sub-nodes) pointed by the cursor,
    if they are already stored in the storage.
    
    If the contents are not yet stored in the storage, it does nothing.
*)
