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
type t 
(** Type of handle *)

val empty_cursor : t -> Cursor.t

val roots : t -> Roots.t
val context : t -> Context.t

val create : 
  ?context_pos: int64 
  -> ?context_length: int 
  -> ?hashcons: Hashcons.config
  -> string
  -> t

val open_ : 
  mode: Storage.mode
  -> ?context_pos: int64 
  -> ?hashcons: Hashcons.config
  -> string
  -> t

val close : t -> unit

val checkout : t -> Roots.RootHash.t -> Cursor.t option

(* XXX option order is awful *)
val commit 
  : ?allow_missing_parent: bool 
  -> ?override: bool 
  -> Bud_cache.t 
  -> t 
  -> parent: Roots.RootHash.t option 
  -> meta:string 
  -> ?hash_override: Roots.RootHash.t 
  -> Cursor.t 
  -> Cursor.t * Hash.t
(** Commit a hash.  If [override] is [false] (by default), hash collision fails the function.  If it is [true], it overwrites the hash.
*)
