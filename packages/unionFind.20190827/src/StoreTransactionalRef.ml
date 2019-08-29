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

(* A reference cell records both its current (possibly uncommitted) value and
   its last committed value. A cell is considered stable when these two values
   are (physically) equal, and unstable otherwise. *)

(* One could perhaps enrich each cell with a pointer to its store, so as to
   ensure at runtime that the user is not confused. *)

type 'a rref = {
  (* The current (possibly uncommitted) value. *)
  mutable current: 'a;
  (* The last committed value. *)
  mutable committed: 'a
}

(* A transaction contains a stack of all unstable cells (and possibly some
   stable cells too, although that is unlikely). *)

type 'a transaction =
  'a rref Stack.t

(* A store contains an optional transaction. This indicates whether a
   transaction is currently ongoing. Transactions cannot be nested. *)

type 'a store =
  { mutable transaction: 'a transaction option }

let new_store () : 'a store =
  { transaction = None }

let make (s : 'a store) (v : 'a) : 'a store * 'a rref =
  s, { current = v; committed = v }

let get (s : 'a store) (x : 'a rref) : 'a store * 'a =
  s, x.current

let set (s : 'a store) (x : 'a rref) (v : 'a) : 'a store =
  begin match s.transaction with
  | None ->
      (* Outside of a transaction, two normal write operations are performed.
         The cell remains stable. Nothing is logged. *)
      x.current <- v;
      x.committed <- v
  | Some stack ->
      (* We are within a transaction. *)
      let current = x.current in
      (* If the new value happens to be the current value, there is nothing to do. *)
      if v != current then begin
        (* If this cell was stable and now becomes unstable, then it must be
           inserted into the set of unstable cells, which is recorded as part
           of the transaction. *)
        if current == x.committed then
          Stack.push x stack;
        (* The cell must then be updated. If [v] happens to be equal to
           [committed], this could make the cell stable again. We do not
           check for this unlikely situation. This means that the set of
           unstable cells could actually contain stable cells too. *)
        x.current <- v
      end
  end;
  s

let eq (s : 'a store) (x : 'a rref) (y : 'a rref) : 'a store * bool =
  s, x == y

exception NestedTransactionAttempt

let commit x =
  x.committed <- x.current

let rollback x =
  x.current <- x.committed

let tentatively (s : 'a store) (f : unit -> 'b) : 'b =
  match s.transaction with
  | Some _ ->
      raise NestedTransactionAttempt
  | None ->
      let stack = Stack.create() in
      s.transaction <- Some stack;
      try
        let b = f() in
          (* Commit every unstable cell. *)
          Stack.iter commit stack;
          (* Close the transaction. *)
          s.transaction <- None;
          (* Report the outcome. *)
          b
        with e ->
          (* Roll back every unstable cell. *)
          Stack.iter rollback stack;
          (* Close the transaction. *)
          s.transaction <- None;
          (* Report the outcome. *)
          raise e

