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

(* Every cell records both its current (possibly uncommitted) value
   and its last committed value. A cell is considered stable when
   these two values are (physically) equal, and unstable otherwise. *)

type 'a cell = {
  (* The current (possibly uncommitted) value. *)
  mutable current: 'a;
  (* The last committed value. *)
  mutable committed: 'a
}

(* A transaction contains a stack of all unstable cells (and possibly
   some stable cells too, although that is unlikely). *)

type 'a transaction =
    'a cell Stack.t

(* [create v] creates a new cell whose initial value is [v]. *)

let create v = {
  current = v;
  committed = v
}

(* [get cell] is the current value of the cell [cell]. *)

let get cell =
  cell.current

(* [set transaction cell v] sets the value of the cell [cell] to [v]. This is
   done as part of the transaction [transaction]. Thus, if the transaction is
   aborted, this update will be undone. If the transaction is committed, this
   update will be kept. *)

(* If this cell was created after the beginning of the transaction, then there
   is no need to insert it into the set of unstable cells, as it does not need
   to be rolled back. We do not implement this idea. In our intended application,
   cells are never created during a transaction. *)

let set transaction cell v =
  let current = cell.current in
  (* If the new value happens to be the current value, then there is nothing
     to do. *)
  if v == current then
    ()
  else begin
    (* If this cell was stable and now becomes unstable, then it must be
       inserted into the set of unstable cells, which is recorded as part
       of the transaction. *)
    if current == cell.committed then
      Stack.push cell transaction;
    (* The cell must then be updated. If [v] happens to be equal to
       [committed], this could make the cell stable again. We do not
       check for this unlikely situation. This means that the set of
       unstable cells could actually contain stable cells too. *)
    cell.current <- v
  end

(* [tentatively f] runs the function [f] within a new transaction. If [f]
   raises an exception, then the transaction is aborted, and any updates
   performed by [f] are rolled back. Otherwise, the updates performed by [f]
   are committed. *)

let commit cell =
  cell.committed <- cell.current

let rollback cell =
  cell.current <- cell.committed

let tentatively f =
  let transaction = Stack.create() in
  try
    let result = f transaction in
    (* Commit every unstable cell. *)
    Stack.iter commit transaction;
    result
  with e ->
    (* Roll back every unstable cell. *)
    Stack.iter rollback transaction;
    raise e

