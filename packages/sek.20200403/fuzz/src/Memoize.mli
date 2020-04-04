(******************************************************************************)
(*                                                                            *)
(*                                    Sek                                     *)
(*                                                                            *)
(*          Arthur Charguéraud, Émilie Guermeur and François Pottier          *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Lesser General Public License as published by the Free   *)
(*  Software Foundation, either version 3 of the License, or (at your         *)
(*  option) any later version, as described in the file LICENSE.              *)
(*                                                                            *)
(******************************************************************************)

module type MEMOIZER = sig
  (* A type of keys. *)
  type key
  (* A memoization combinator for this type. *)
  val memoize: (key -> 'a) -> (key -> 'a)
  (* A recursive memoization combinator for this type. *)
  val fix: ((key -> 'a) -> (key -> 'a)) -> (key -> 'a)
end

module type IMPERATIVE_MAP = sig
  (* A type of keys. *)
  type key
  (* A type of imperative maps. *)
  type 'a t
  (* Creation, insertion, lookup. *)
  val create: int -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val find: 'a t -> key -> 'a
end

module Make (M : IMPERATIVE_MAP) : MEMOIZER with type key = M.key

module MakeViaMap (O : Map.OrderedType) : MEMOIZER with type key = O.t

module MakeViaHashtbl (H : Hashtbl.HashedType) : MEMOIZER with type key = H.t

module Int : MEMOIZER with type key = int
