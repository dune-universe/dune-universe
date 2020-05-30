(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2019,2020 DaiLambda, Inc. <contact@dailambda.jp>            *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)
(** { 1 Root hash table }

  All the data should be in memory.

  The root hashes are stored in the context.  They are chained and the last 
  root hash entry is recorded in the header of the context.
    
  Note that the chain of root hashes in the context do not correspond with 
  the parent-children relationship of root hashes.

*)

module RootHash : sig
  (** Hash stored in the roots can be different from Plebeia's
      root Merkle hashes.  Its length is 32bytes.
  *)
  
  type t
  val of_string       : string -> t
  val of_plebeia_hash : Hash.t -> t
  val to_string       : t -> string
  val to_hex_string   : t -> string
end
  
module Entry : sig
  type t = 
    { meta   : string (* Log message 20 bytes *)
    ; prev   : Index.t option (* Previous entry index *)
    ; parent : Index.t option (* Index of the parent root node *)
    ; index  : Index.t (* Index of the root node *)
    ; hash   : RootHash.t (* Context hash *)
    }
end

type entry = Entry.t = 
  { meta   : string (* Log message 20 bytes *)
  ; prev   : Index.t option (* Previous entry index *)
  ; parent : Index.t option (* Index of the parent root node *)
  ; index  : Index.t (* Index of the root node *)
  ; hash   : RootHash.t (* Context hash *)
  }
  
type t
(** Storage type *)

val create : storage_context: Storage.t -> storage_roots: Storage.t -> t
(** Load root hashes of the context and return t *)

val read_additional_commits : t -> (int, unit) Result.t
(** Reload additional commits from the updated context.

    Readers must call [Storage.sync] prior to it in order to load new
    roots added by the Writer.
*)

val sync : t -> unit
(** For reader, to update the root table informat which may be updated
    by the writer *)

val add : t -> ?parent: Index.t -> RootHash.t -> Index.t -> meta:string -> unit
(** Add a new root hash, with its index, parent, meta commit log.

    It immediately saves the root to the context.

    If the root hash already exists in the table, it is overridden 
    with the new one by [add].
*)

val mem : t -> RootHash.t -> bool
(** Existence check.  O(1) using hash table  *)

val find : t -> RootHash.t -> Entry.t option
(** Find a root of the given hash.  O(1) using hash table  *)

val find_by_index : t -> Index.t -> Entry.t option
(** Find by index.  O(1) using hash table *)

val genesis : t -> Entry.t list
(** Returns the hashes which have no parents.  O(n) *)

val children : t -> Entry.t -> Entry.t list
(** Returns the childlren of the entry.  O(1) using hash table *)

val fold : (Entry.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
(** folding.  The entries are applied in the order of registration,
    from the eldest to the newest.
*)

val fold_breadth_first : (Entry.t -> 'acc -> 'acc) -> t -> 'acc -> 'acc
(** Breadth first iteration of roots from genesis to the latest.
    Parents are always called before their children.
*)

val iter : (Entry.t -> unit) -> t -> unit
(** Iteration *)

val length : t -> int
(** The number of entries in the table *)

val to_seq : t -> Entry.t Seq.t
(** The entries in the table.  The ordering is not specified. *)
    
val get_latest : t -> Entry.t option
(** Get the latest addition *)
