(***************************************************************************)
(*                                                                         *)
(*                                 UnionFind                               *)
(*                                                                         *)
(*                       François Pottier, Inria Paris                     *)
(*                                                                         *)
(*  Copyright Inria. All rights reserved. This file is distributed under   *)
(*  the terms of the GNU Library General Public License version 2, with a  *)
(*  special exception on linking, as described in the file LICENSE.        *)
(***************************************************************************)

(* A signature for first-class stores. *)

module type STORE = sig

  (* A store can be thought of as a region of memory in which objects, known
     as references, can be dynamically allocated, read and written. Here, we
     require stores to be homogeneous -- i.e., all references have the same
     content type -- because this allows a wider range of implementations. *)
  type 'a store

  (* Creating a new store. *)
  val new_store: unit -> 'a store

  (* A reference can be thought of as (a pointer to) an object that exists in
     some store. *)
  type 'a rref

  (* The type parameter ['a] in ['a rref] could be considered redundant, as it
     is not really necessary that both [store] and [rref] be parameterized.
     However, one can think of instances where ['a store] is a phantom type
     and ['a rref] really depends on ['a] AND of instances where the converse
     holds. *)

  (* For regularity, each of the four operations below takes a store as a
     parameter and returns a store as a result. One might think that [eq]
     does not need a store parameter, and that [get] and [eq] do not need
     a store result. However, in some implementations where the store is
     self-organizing, this may be necessary, so we bite the bullet and pay
     the cost in runtime and verbosity. *)

  (* Creating a new reference. *)
  val make: 'a store -> 'a -> 'a store * 'a rref

  (* Reading a reference. *)
  val get:  'a store -> 'a rref -> 'a store * 'a

  (* Writing a reference. *)
  val set:  'a store -> 'a rref -> 'a -> 'a store

  (* Comparing two references for physical equality. The two references must
     belong to the same store; the result of this operation is otherwise
     undefined. *)
  val eq: 'a store -> 'a rref -> 'a rref -> 'a store * bool

end
