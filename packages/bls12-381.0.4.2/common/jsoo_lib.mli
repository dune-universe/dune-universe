(*****************************************************************************)
(*                                                                           *)
(* Copyright (c) 2020-2021 Danny Willems <be.danny.willems@gmail.com>        *)
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

open Js_of_ocaml

module type JS_OBJECT = sig
  type t

  val of_js : Js.Unsafe.any -> t

  val to_any_js : t -> Js.Unsafe.any

  (** Equivalent to [toString] on the object in JavaScript *)
  val to_string : t -> string
end

module Number : sig
  include JS_OBJECT

  val zero : t

  (** Create a [Number] value from an integer. The value is not verified to be in
  the interval allowed for a [Number] object *)
  val of_int : int -> t

  (** Convert to [int]. Unsafe *)
  val to_int : t -> int
end

module ArrayBuffer : sig
  include JS_OBJECT

  val make : int -> char -> t

  val is_array_buffer : Js.Unsafe.any -> bool

  val length : t -> Number.t
end

module type TYPED_ARRAY = sig
  include JS_OBJECT

  (** OCaml type to represent the elements of the typed array. *)
  type elt

  (** Name of the typed array. Example: [Uint8Array], [Uint16Array] *)
  val name : string

  val create : ?offset:Number.t -> ?length:Number.t -> ArrayBuffer.t -> t

  (** [buffer a] returns the underlying buffer of the array. Equivalent to
      [a.buffer] in JavaScript *)
  val buffer : t -> ArrayBuffer.t

  (** [byte_length a] returns the number of bytes of the typed array. Equivalent to [a.byteLength] in JavaScript *)
  val byte_length : t -> Number.t

  (** [set a i x] is equivalent to a[i] = x in JavaScript *)
  val set : t -> int -> elt -> t

  (** [slice array a b] returns [array[a], ..., array[b]] *)
  val slice : t -> int -> int -> t
end

(** Binding to Uint8Array.
    Compatible with >= ES6 only
 *)
module Uint8TypedArray : sig
  include
    TYPED_ARRAY
      with type elt = int
       and type t = Js_of_ocaml.Typed_array.uint8Array Js.t

  (** Convert to bytes *)
  val to_bytes : t -> bytes
end

(** Represent a ES module. Use [ESModule.of_js m] where [m] is [Js.Unsafe.js_expr {|require ("module_name") |}] *)
module ESModule : sig
  include JS_OBJECT with type t = Js.Unsafe.any
end

module Memory : sig
  module Buffer = Uint8TypedArray

  val copy_in_buffer : Buffer.t -> Bytes.t -> int -> int -> int -> unit
end
