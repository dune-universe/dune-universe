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

(** This module offers an implementation of owners. *)

(** The type [owner] represents the identity of a data structure
    owner. *)

(** The type [owner] is equipped with a total order. The specifications of
    [none], [above], and [join] involve on this order. *)

type owner

(** The distinguished owner [none] represents the absence of an owner.
    Therefore, it indicates that a data structure is shared. [none] is
    the minimum element of the total order. *)

val none: owner

(** [zero] is a fixed owner, distinct from [none]. *)

val zero: owner

(** [above o] returns an owner that lies strictly above [o], that is,
    an owner [o'] such that [o < o'] holds. [o] must be distinct from
    [none]. *)

val above: owner -> owner

(** [join o1 o2] returns an owner that lies above [o1] and [o2], that
    is, an owner [o'] such that [o1 <= o'] and [o2 <= o'] hold. [o1]
    and [o2] must be distinct from [none]. *)

val join: owner -> owner -> owner

(** [is_uniquely_owned o1 o2] determines whether [o1] and [o2] are the same
    owner and are distinct from [none]. This is used to test whether a data
    structure tagged with creator [o1] is uniquely owned by [o2]. *)

val is_uniquely_owned: owner -> owner -> bool

(** [show o] produces a string representation of the owner [o]. It is
    used for debugging purposes only. *)

val show: owner -> string

(** [leq o1 o2] tests the ordering relation [o1 <= o2]. It is used for
    debugging purposes only. *)

val leq: owner -> owner -> bool
