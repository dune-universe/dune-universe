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
(** Deprecated.  Do not use it any more *)

(** 
   Search Bud or Leaf by index.

   Note that at worst case this can fall into total searches, 
   which can be VERY SLOW.  You must first find alternatives to avoid 
   to use this module.

   The total search is avoided using the property that nodes have indices
   larger than those of children.  If the property breaks, the search
   defined in this module stops working correctly.

   It is so far used to find the source of copied nodes.  In Tezos,
   almost of all the cases the source is /data/rolls/owner/current 
   and it is always found less than 0.01 seconds.
*)

open Node
open Segment

type t 
(** Search state *)

val from : Cursor.t -> t
(** Build a search state starting from the given cursor *)  

val run : t -> node -> (Segs.t option * t, string) Result.t
(** Search a Bud or Leaf with the given index from the cursor used to build
    the search state [t].
    It returns the first node it finds. 
    
    The index must be of a Bud or a Leaf.
    The tree under the cursor must be fully indexed.
*)
