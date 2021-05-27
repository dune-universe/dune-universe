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

(* This is roughly a subset of OCaml's standard signature [Map.S]. *)

(* The functions that have been removed (because some of our candidate
   implementations do not provide them) are
   [min_binding], [max_binding], [min_binding_opt], [max_binding_opt],
   [split], [partition],
   [merge],
   [for_all],
   [find_first], [find_first_opt], [find_last], [find_last_opt],
   [to_seq], [to_seq_from], [add_seq], [of_seq]. *)

(* The type of [union] has been changed so as to make it less general. Instead
   of accepting a function of type [key -> 'a -> 'a -> 'a option], it requires
   a function of type ['a -> 'a -> 'a]. *)

(* The declaration of the type [t] has been changed from [+'a t] to ['a t],
   and the type of [empty] has been changed from ['a t] to [int t], in order
   to accommodate [BatIMap], which does not satisfy the stronger interface. *)

module type S =
  sig
    type key
    type 'a t
    val empty: int t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val choose: 'a t -> key * 'a
    val choose_opt: 'a t -> (key * 'a) option
    val find: key -> 'a t -> 'a
    val find_opt: key -> 'a t -> 'a option
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  end
