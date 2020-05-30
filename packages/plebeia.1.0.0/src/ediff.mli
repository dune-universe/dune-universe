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
(** Bud and leaf based diff. *)

open Utils
open Node
open Segment

type t = 
  | ModLeaf     of node * node * Segs.t 
    (* Leaf modification: 
       [ModLeaf (old_node, new_node, segs_to_the_nodes)] 
    *)

  | CleanBud    of node * Segs.t 
    (* Bud is emptied.  The bud itself is not removed:
       [CleanBud (old_bud, segs_to_the_bud)]
    *)

  | Del         of node * Segs.t 
    (* Bud/Leaf deletion:
       [Del (old_node, segs_to_the_old_node)]
    *)
       
  | CopyBud     of Segs.t * node * Segs.t
    (* Bud copy:
       [ CopyBud (segs_to_the_bud, the_source_bud, segs_to_the_destination) ]
    *)

  | NewEmptyBud of node * Segs.t 
    (* Bud is emptied:
       [ NewEmptyBud (the_old_bud, segs_to_the_bud) ]
    *)

  | CopyLeaf    of Segs.t * node * Segs.t
    (* Leaf copy:
       [ CopyLeaf (segs_to_the_old_leaf, the_source_leaf, segs_to_the_destination) ]
    *)

  | NewLeaf     of node * Segs.t
    (* New leaf creation
       [ NewLeaf (new_node, segs_to_the_new_node)]
    *)

  | SmallLeaf   of node * Segs.t
    (* New small leaf
       [ SmallLeaf (new_node, segs_to_the_new_node) ]
    *)                            

val pp : Format.formatter -> t -> unit
(** Pretty printing *)

val apply : Cursor.t -> t -> Cursor.t
(** Application of a diff *)

val diff : Context.t -> node -> node -> t list
(** [diff src dst] gets the segment based diff between 2 nodes *)

val check : t list -> Context.t -> node -> node -> unit
(** [check diffs context n1 n2] checks the correctness of [diffs]
    between [n1] [n2] by applying them to [n1] and comparing 
    the hashes of the application result and [n2].

    If hashes are not equal, it prints an error message and raises
    [Assert_failure].

    [n2] must be already hashed.
*) 

val expand_bud_deletions : Context.t -> t -> t list
(** Each directory removal is converted to file removals 

    It is "pseudo" since information of CleanBud is lost. 
*)

  
