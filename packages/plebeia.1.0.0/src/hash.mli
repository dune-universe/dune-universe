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
(** { 1 The first half 224bit/28byte part of node hash. } *)

(** { 2 The type } *)

type t = private string
(** Type for the hash.  28 bytes *)

val zero : t
(** 28 bytes of 0's *)

(** { 2 Printer and parser } *)

val to_string : t -> string
(** Returns a binary string representation of hash *)

val of_string : string -> t
(** Create a hash from 28 bytes binary string.
    String with an invalid length raises [Assert_failure] *)

val to_hex : t -> Hex.t
val to_hex_string : t -> string

val of_hex : Hex.t -> t
(** Create a hash from a hexiadecimal representation of 28 bytes binary string.
    Input with an invalid length raises [Assert_failure] *)

val encoding : t Data_encoding.t
(** Data encoding *)

(** { 2 Hash computation } *)

val hash : string -> t
(** Compute the hash of the given binary string *)

val hash_list : string list -> t
(** Compute the hash of the concatenation of the given binary strings *)

(** { 2 Manipulation } *)

val set_last_2bits : int -> t -> t
(** set the last 2 bits of the hash to the given integer *)

val test : unit -> unit
(** check the correctness of blake2b *)
