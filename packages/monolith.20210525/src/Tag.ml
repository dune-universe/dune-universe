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

open Eq

(* The type [_ tag] is declared as an extensible type. *)

(* For type safety, this type must be invariant. *)

type _ tag = ..

let new_tag (type a) () : a tag =
  (* Extend the type [tag] with a new data constructor [Tag]. *)
  let module T = struct
    type _ tag +=
      | Tag : a tag
  end in
  (* Return this data constructor. *)
  T.Tag

(* The tag equality test is OCaml's address equality. This is the most precise
   equality that one can think of, yet it is not too precise: indeed, the only
   way in which the user might get hold of two physically distinct tags is via
   two distinct calls to [new_tag()], and the tags returned by two such calls
   must be considered logically different. *)

(* We must use an unsafe cast [Obj.magic] to convince the type-checker that
   address equality [==] can be given the type advertised in [Tag.mli]. This
   type seems intuitively safe because there is no way of changing the type of
   a tag after it has been created; thus, if a single tag has both type [t1
   tag] and [t2 tag], then the types [t1] and [t2] must be equal. The fact
   that the type [_ tag] is invariant is exploited in this argument. *)

(* A well-typed way of implementing a tag equality test would be to use a
   linear cascade of binary comparisons. Unfortunately, this approach involves
   global state (because the equality function must be patched every time a
   new tag is created) and has linear time complexity, so it is not
   attractive. *)

exception RuntimeTagError

let equal tag1 tag2 =
  if tag1 == Obj.magic tag2 then Obj.magic Eq else raise RuntimeTagError
