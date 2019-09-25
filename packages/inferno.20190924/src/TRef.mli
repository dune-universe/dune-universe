(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

(* This module provides a simple-minded of transactional reference cells.
   Like ordinary references, these references be created, read, and
   written. In addition, they support a notion of transaction. All writes must
   take place within a transaction. At the end of the transaction, the user
   chooses to either commit or roll back all of the write operations that took
   place during the transaction. *)

(* The type of cells. *)

type 'a cell

(* The type of transactions. The [set] operation must be passed the current
   transaction. *)

type 'a transaction

(* [create v] creates a new cell whose initial value is [v]. *)

val create: 'a -> 'a cell

(* [get cell] is the current value of the cell [cell]. *)

val get: 'a cell -> 'a

(* [set transaction cell v] sets the value of the cell [cell] to [v]. This is
   done as part of the transaction [transaction]. Thus, if the transaction is
   aborted, this update will be undone. If the transaction is committed, this
   update will be kept. *)

val set: 'a transaction -> 'a cell -> 'a -> unit

(* [tentatively f] runs the function [f] within a new transaction. If [f]
   raises an exception, then the transaction is aborted, and any updates
   performed by [f] are rolled back. Otherwise, the updates performed by [f]
   are committed. The transaction argument to [f] must not be retained beyond
   the execution of [f]. Transactions must not be nested. *)

val tentatively: ('a transaction -> 'b) -> 'b

