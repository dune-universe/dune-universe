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

(** This module offers a union-find data structure based on disjoint set
    forests, with path compression and linking by rank. *)

(** Instead of hard-coding the use of mutable state, we parameterize
    the module over an implementation [S] of stores, as described by
    the signature [STORE]. This allows the client to choose between
    many different representations of the store: e.g., based on
    primitive references, based on a (possibly extensible) primitive
    array, based on a persistent map, based on a persistent or
    semi-persistent array, based on transactional references, etc. *)

open Store

(** The signature provided by this module is also [STORE], extended with an
    operation for merging two references. *)

module Make (S : STORE) : sig

  (** The new store is implemented on top of the store provided by the user.
      This means that any extra operations supported by the underlying store
      (such as, say, opening and committing a transaction) are applicable to
      this new store as well. *)
  type 'a content

  (** Operations for creating a new store, creating a new reference,
      reading and writing a reference, and comparing two references.
      Two references are considered equal after they have been merged
      by [union]. *)
  include STORE
    with type 'a store = 'a content S.store
     and type 'a rref = 'a content S.rref

  (** [union f s x y] does nothing if the references [x] and [y] are equal,
      and merges them if they are distinct. In the latter case, the new content
      of the merged reference is computed by applying the user-supplied
      function [f] to the original contents of [x] and [y]. *)

  val union: ('a -> 'a -> 'a) -> 'a store -> 'a rref -> 'a rref -> 'a store

end
