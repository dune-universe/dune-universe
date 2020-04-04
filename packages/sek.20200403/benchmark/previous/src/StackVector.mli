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

open Sek

(** Signature of polymorphic ephemeral stacks *)

module type ResizeFactorsSig =
sig
  val min_capacity : int
  val grow_factor : int
  val shrink_condition : int
  val shrink_factor : int
end

module Factors2 : ResizeFactorsSig

module Factors4 : ResizeFactorsSig


(** Stack implemented as a vector (resizable array).

    The ResizeFactors argument specifies the resizing policy.
    By default, one should use [Factors2],
    to double size when full, and to half size when less than
    quarter full.

*)

module[@inline] Make
  (ResizeFactors : ResizeFactorsSig)
  (OverwriteEmptySlots : OVERWRITE_EMPTY_SLOTS)
: sig
  type 'a t
  val length: 'a t -> int
  val create: 'a -> 'a t
  val is_empty: 'a t -> bool
  val push: 'a -> 'a t -> unit
  val top: 'a t -> 'a
  val pop: 'a t -> 'a
  val get: 'a t -> int -> 'a
  val set: 'a t -> int -> 'a -> unit
  val iter: ('a -> unit) -> 'a t -> unit
  val fold_left: ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  val fold_right: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val to_list: 'a t -> 'a list
end
