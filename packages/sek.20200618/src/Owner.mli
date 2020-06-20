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

(** The type [owner] represents the identity of a data structure owner. *)
type owner

(** The distinguished owner [none] represents the absence of an owner.
    Therefore, it indicates that a data structure is shared. *)
val none: owner

(** [zero] is a fixed owner, distinct from [none]. *)
val zero: owner

(** [fresh()] returns a fresh owner. *)
val fresh: unit -> owner

(** [is_uniquely_owned o1 o2] tests whether a data structure tagged with
   creator [o1] is uniquely owned by [o2]. *)
val is_uniquely_owned: owner -> owner -> bool

(** [show o] produces a string representation of the owner [o]. It is
    used for debugging purposes only. *)
val show: owner -> string
