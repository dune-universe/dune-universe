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
(** { 1 Cache for small values } 

    This module provides hash-consing of small Leaf values
    under [max_leaf_size] of [config].
*)

type t

(** { 2 Configuration } *)

type config = 
  { max_leaf_size : int 
    (** Maximum size of leaf value stored *)
  ; max_words : int
  }

val default_config : config

val config : t -> config
(** Get the config *)

(** { 2 Table } *)

val create : config -> t
(** Create an empty hashcons table.  
    To load the existing hashconsed table from the storage, 
    [read] must be used.
*)

val find : t -> Value.t -> (Index.t option, Error.t) Result.t
(** Find the hasheconsed value from the table *)

val add : t -> Value.t -> Index.t -> (unit, Error.t) Result.t
(** Register a value from the table *)

(** { 2 Statistics } *)

val stat : Format.formatter -> t -> unit
(** Print out statistics *)

(*
val size : t -> int
(** Very rough size of the table in bytes *)
*)

val may_shrink : t -> unit
