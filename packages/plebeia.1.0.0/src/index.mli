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
(** { 1 Index in data files } *)

type t

(** { 2 Constants } 

    The zero index is treated specially.
*)

val zero : t
val one : t
val max_int : t
  
val zero_then_none : t -> t option
(** This function returns [None] if [t] is zero. *)

(** { 2 Arithmetics } *)

val pred : t -> t
val succ : t -> t
val (-) : t -> t -> t
val (+) : t -> t -> t
val ( * ) : t -> t -> t
val (/) : t -> t -> t

val compare : t -> t -> int

(** { 2 Conversion to integer types } *)

val to_int32 : t -> int32
val of_int32 : int32 -> t

val to_int64 : t -> int64
val of_int64 : int64 -> t

val of_int : int -> t

val to_int : t -> int
(** Be careful to use it in the 32-bit arch, where OCaml's [int] is 31-bits,
    since [t] is unsigned 32bit integer. 
*)

val of_uint32 : Stdint.Uint32.t -> t
val to_uint32 : t -> Stdint.Uint32.t
  
(** { 2 Set and Map } *)

module Set : Set.S with type elt = t
module Map : Map.S with type key = t
