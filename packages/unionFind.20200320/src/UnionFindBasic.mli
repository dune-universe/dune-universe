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

(** A type of elements, also known as references. *)

type 'a elem

(** Operations for creating a new reference, reading and writing a reference,
    and testing whether two references are equivalent. (Two references are
    considered equivalent after they have been merged by [union].) *)

val make: 'a -> 'a elem
val get: 'a elem -> 'a
val set: 'a elem -> 'a -> unit
val eq: 'a elem -> 'a elem -> bool

(** [union x y] does nothing if the references [x] and [y] are equal, and
    merges them if they are distinct. In the latter case, the content of the
    merged reference is arbitrarily chosen to be the previous content of [x]
    or [y]. *)

val union: 'a elem -> 'a elem -> 'a elem

(** [find x] returns the representative element of [x]'s equivalence class. *)

val find: 'a elem -> 'a elem
