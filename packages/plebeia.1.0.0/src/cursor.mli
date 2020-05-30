(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Arthur Breitman <arthur.breitman+nospam@tezos.com>     *)
(* Copyright (c) 2019 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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
(** { 1 Zipper } *)

open Node

type Error.t += Write of string

(** { 2 Types: Trail and cursor } *)

type modified =
  | Modified
  | Unmodified of indexed * hashed

(** A trail represents the content of the memory stack when recursively
    exploring a tree.  Constructing these trails from closure would be easier,
    but it would make it harder to port the code to C. The type parameters of
    the trail keep track of the type of each element on the "stack" using 
    a product type. 

    The constructors are private.  Use '_' prefixed functions with runtime
    invariant checks.
*)
type trail = private
  | Top
  | Left of (* we took the left branch of an internal node *)
      trail
      * node (* the right node *)
      * modified

  | Right of (* we took the right branch of an internal node *)
      node (* the left node *)
      * trail
      * modified

  | Budded of
      trail
      * modified

  | Extended of
      trail
      * Segment.t
      * modified

type cursor = private
    Cursor of trail
              * node
              * Context.t
(** The cursor, also known as a zipper combines the information contained in a
   trail and a subtree to represent an edit point within a tree. This is a
   functional data structure that represents the program point in a function
   that modifies a tree. We use an existential type that keeps the .mli sane
   and enforces the most important: that the hole tags match between the trail
   and the Node *)

type t = cursor

(** { 2 Constructor with invariant checks } *)

val _Top : trail
val _Left : trail * node * modified -> trail
val _Right : node * trail * modified -> trail
val _Budded : trail * modified -> trail
val _Extended : trail * Segment.t * modified -> trail
val _Cursor : (trail * node * Context.t) -> cursor

(** { 2 Creation } *)

val empty : Context.t -> t
(** Creates a cursor to a new, empty tree. *)

(** { 2 Accessors } *)

val context : t -> Context.t

val index : t -> Index.t option
(** Get the index of the node pointed by the cursor, if indexed. *)

(** { 2 Segments } *)
    
val segs_of_trail : trail -> Segment.t list
(** Segment side list of the given trail, splitted by buds *)

val segs_of_cursor : t -> Segment.t list
(** Segment side list of the given cursor, splitted by buds *)

val local_seg_of_trail : trail -> Segment.t
(** Segment side list of the given trail, splitted by buds *)

val local_seg_of_cursor : t -> Segment.t
(** Segment side list of the given cursor, splitted by buds *)

(** { 2 View } *)

val view : t -> t * view
(** Get the view of the cursor.  Returns also the updated cursor with
    the view. *)

val may_forget : t -> t option
(** If the node pointed by the cursor is indexed, forget the details *)

(** { 2 Zipper functions } *)

(** Result of access_gen *)
type access_result =
  | Empty_bud 
      (* The bud is empty *)
  | Collide of cursor * view 
      (* The segment was blocked by an existing leaf or bud *)
  | Middle_of_extender of cursor * Segment.t * Segment.t * Segment.t 
      (* The segment ends or diverges at the middle of an Extender with the common prefix,
         the remaining extender, and the rest of segment *)
  | Reached of cursor * view 
      (* just reached to a node *)

type Error.t += 
  | Access of access_result
  | Move of string

val error_access : access_result -> ('a, Error.t) Result.t
(** Make an access result into an error *)
(* XXX access_result -> Error.t ? *)

(** { 3 Simple 1 step cursor movement } *)
  
val go_below_bud : t -> (t option, Error.t) Result.t
(** This function expects a cursor positionned on a bud 
    and moves it one step below. *)

val go_down_extender : t -> (t, Error.t) Result.t
(** Go down an Extender node.  The cursor must point to an Extender. *)
    
val go_side : Segment.side -> t -> (t, Error.t) Result.t
(** Go down an Internal node.  The cursor must point to an Internal. *)

val go_up : t -> (t, Error.t) Result.t
(** Go up one level *)

(** { 3 Complex multi step cursor movement } 

    Many of these functions fail when the given cursor does not point 
    to a bud.
*)
    
val access_gen : t -> Segment.t -> (access_result, Error.t) Result.t
(** Follow a segment.  [t] must point to a bud. The function first
    go below the bud, then follow the segment.
*)

val access_gen' : t -> Segment.t -> (access_result, Error.t) Result.t
(** Follow a segment.  [t] can be any node. *)

val go_top : t -> (t, Error.t) Result.t
(** Move up to the top *)

val go_up_to_a_bud : t -> (t, Error.t) Result.t
[@@deprecated "Use go_up_to_bud, which has better semantics"]
(** Moves the cursor back to the bud above.  
    Note that this is not like "cd ../".  

    If the cursor is already  at a bud, it does not move it.
    This is super confusing.  Do not use this function.
*)

val go_up_to_bud : t -> (t, Error.t) Result.t
(** Moves the cursor back to the bud above.  
    Note that this is not like "cd ../".  

    If the cursor is already at a bud, the cursor will move to its parent bud.
*)

val parent : t -> (t, Error.t) Result.t
(** Moves the cursor back to the bud above.  Like "cd ../".
    The cursor must point to a bud otherwise [parent] fails.
*)

val subtree : t -> Segment.t -> (t, Error.t) Result.t
(** Moves the cursor down a segment, to the root of a sub-tree. Think
    "cd segment/" *)

val create_subtree: t -> Segment.t -> (t, Error.t) Result.t
(** Create a subtree (bud). Think "mkdir segment".
    The cursor does NOT move from the original position. *)

val subtree_or_create : t -> Segment.t -> (t, Error.t) Result.t
(** Same as subtree but create a subtree if not exists *)

val get : t -> Segment.t -> (t * [`Leaf of view | `Bud of view], Error.t) Result.t
(** Gets a value if present in the current tree at the given
    segment. *)

val get_value : t -> Segment.t -> (t * Value.t, Error.t) Result.t
(** Gets a value or a bud at the given segment. *)

val insert: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
(** Inserts a value at the given segment in the current tree.
    The cursor does NOT move from the original position. *)

val upsert: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
(** Upserts. This can still fail if the segment leads to a subtree.
    The cursor does NOT move from the original position. *)

val update: t -> Segment.t -> Value.t -> (t, Error.t) Result.t
(** Update. A value must be bound at the segment. *)

val delete: t -> Segment.t -> (t, Error.t) Result.t
(** Delete a leaf or subtree.
    The cursor does NOT move from the original position. *)

val remove_empty_bud : t -> (t, Error.t) Result.t
(** Remove the empty Bud pointed by the cursor.  If the non-root parent 
    becomes empty by the removal, [remove_empty_bud] recursively removes it, 
    too.  If the cursor points non empty bud, it does nothing.
    
    The cursor in the result points to the parent bud of the upmost removed 
    bud.
*)

val alter : 
  t ->
  Segment.segment ->
  (view option -> (node, Error.t) Result.t) -> (t, Error.t) Result.t
(** [alter] can easily break the invariants. *)

val fold : 
  init:'acc 
  -> t 
  -> ('acc -> t -> [< `Continue | `Exit | `Up ] * 'acc) 
  -> 'acc
(** Folding over the node tree.  The function can choose the node traversal 
    from the given cursor: either continuing into its sub-nodes, 
    not traversing its sub-nodes, or quit the entire folding.
    
    If a node is shared at multiple places it is visited MORE THAN ONCE.
    If you want to avoid visiting a shared node at most once, carry
    a set of visited nodes by indices and check a node is visited or not.
*)

val traverse : 
  'a 
  -> t list 
  -> ('a -> t -> [< `Exit | `Up | `Continue ] * 'a) 
  -> 'a * t list
(** More generic tree traversal than [fold], which has a step-by-step 
    visitor interface: it does not recurse the structure by itself.

    If a node is shared at multiple places it is visited MORE THAN ONCE.
*)
  
(** { 2 Statistics } *)

val stat : t -> Stat.t

(** { 2 Debug } *)
                  
val dot_of_cursor_ref : (t -> string) ref
(** Placeholder of Graphviz rendering of cursors *)
