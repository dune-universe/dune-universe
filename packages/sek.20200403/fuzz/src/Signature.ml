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

(* A signature that describes an implementation of sequences. *)

(* Both the reference implementation and the candidate implementation
   must satisfy this signature. *)

module type SEQUENCES = sig
  type side
  val front: side
  val back : side
  type direction
  val forward  : direction
  val backward : direction
  module Ephemeral : sig
    type 'a t
    val create: 'a -> 'a t
    val make: 'a -> int -> 'a -> 'a t
    val init: 'a -> int -> (int -> 'a) -> 'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val assign: 'a t -> 'a t -> unit
    val push: side -> 'a t -> 'a -> unit
    val pop_opt: side -> 'a t -> 'a option
    val peek_opt: side -> 'a t -> 'a option
    val get: 'a t -> int -> 'a
    val set: 'a t -> int -> 'a -> unit
    val concat: 'a t -> 'a t -> 'a t
    val append: side -> 'a t -> 'a t -> unit
    val split : 'a t -> int -> 'a t * 'a t
    val carve: side -> 'a t -> int -> 'a t
    val iter : direction -> ('a -> unit) -> 'a t -> unit
    val iteri : direction -> (int -> 'a -> unit) -> 'a t -> unit
    val of_array_segment : 'a -> 'a array -> int -> int -> 'a t
    val of_array : 'a -> 'a array -> 'a t
    val to_array : 'a t -> 'a array
    val check: 'a t -> unit
  end
  module Persistent : sig
    type 'a t
    val create: 'a -> 'a t
    val make: 'a -> int -> 'a -> 'a t
    val init: 'a -> int -> (int -> 'a) -> 'a t
    val length: 'a t -> int
    val is_empty: 'a t -> bool
    val push: side -> 'a t -> 'a -> 'a t
    val pop_opt: side -> 'a t -> 'a option * 'a t
    val peek_opt: side -> 'a t -> 'a option
    val get: 'a t -> int -> 'a
    val set: 'a t -> int -> 'a -> 'a t
    val concat: 'a t -> 'a t -> 'a t
    val split: 'a t -> int -> 'a t * 'a t
    val iter : direction -> ('a -> unit) -> 'a t -> unit
    val iteri : direction -> (int -> 'a -> unit) -> 'a t -> unit
    val of_array_segment : 'a -> 'a array -> int -> int -> 'a t
    val of_array : 'a -> 'a array -> 'a t
    val to_array : 'a t -> 'a array
    val check: 'a t -> unit
  end
  val snapshot            : 'a Ephemeral.t -> 'a Persistent.t
  val edit                : 'a Persistent.t -> 'a Ephemeral.t
  val snapshot_and_clear: 'a Ephemeral.t -> 'a Persistent.t
  module E = Ephemeral
  module P = Persistent
end
