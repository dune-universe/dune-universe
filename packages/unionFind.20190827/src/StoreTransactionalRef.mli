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

open Store

(* This module offers an implementation of [STORE] based on a simple form
   of mutable transactional references. *)

include STORE

(* [tentatively s f] runs the function [f] within a new transaction on the
   store [s]. If [f] raises an exception, then the transaction is aborted,
   and any updates performed by [f] are rolled back. Otherwise, the updates
   performed by [f] are committed. *)

(* Two transactions on a single store cannot be nested. *)

(* A cell that is created during a transaction still exists after the
   transaction, even if the transaction is rolled back. In that case,
   its content should be considered undefined. *)

val tentatively: 'a store -> (unit -> 'b) -> 'b

