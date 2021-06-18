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

  val to_string : t -> string
end

module Js_object_base : JS_OBJECT = struct
  type t = Js.Unsafe.any

  let of_js x = x

  let to_any_js x = x

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])
end

module Number = struct
  include Js_object_base

  let of_int x =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr "Number")
      [| Js.Unsafe.inject (Js.string (string_of_int x)) |]

  let zero = of_int 0

  let to_int x =
    int_of_string (Js.to_string (Js.Unsafe.meth_call x "toString" [||]))
end

(* Is already available in Js_of_ocaml.Typed_array. Higher interface with non-JS types *)
module ArrayBuffer = struct
  include Js_object_base

  let make size _c =
    (Js.Unsafe.new_obj (Js.Unsafe.variable "ArrayBuffer"))
      [| Number.to_any_js (Number.of_int size) |]

  let is_array_buffer x =
    Js.to_string (Js.typeof x) = "object"
    && Js.to_string (Js.Unsafe.get (Js.Unsafe.get x "constructor") "name")
       = "ArrayBuffer"

  let length x = Number.of_js (Js.Unsafe.get x "byteLength")
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

(* https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Uint8Array *)
(* The name must be different than Uint8Array if name are kept, otherwise when
   compiled to JavaScript, Uin8Array will be resolved to this module.
 *)
module Uint8TypedArray = struct
  type elt = int

  type t = Js_of_ocaml.Typed_array.uint8Array Js.t

  let name = "Uint8Array"

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])

  let to_any_js x = Js.Unsafe.inject x

  let create ?(offset = Number.zero) ?length array_buffer =
    (* TODO: add checks on parameters *)
    let params =
      if Option.is_none length then
        [| ArrayBuffer.to_any_js array_buffer; Number.to_any_js offset |]
      else
        [| ArrayBuffer.to_any_js array_buffer;
           Number.to_any_js offset;
           Number.to_any_js (Option.get length)
        |]
    in
    (Js.Unsafe.new_obj (Js.Unsafe.variable name)) params

  let of_js x =
    let constructor_name =
      Js.to_string (Js.Unsafe.get (Js.Unsafe.get x "constructor") "name")
    in
    if constructor_name = name then Js.Unsafe.coerce x
    else if constructor_name = "ArrayBuffer" then create (ArrayBuffer.of_js x)
    else
      raise
        (Invalid_argument
           (Printf.sprintf
              "The parameter is of type %s and ArrayBuffer or %s is expected"
              constructor_name
              name))

  let buffer x = ArrayBuffer.of_js (Js.Unsafe.get x "buffer")

  let byte_length x = Number.of_js (Js.Unsafe.get x "byteLength")

  let set x i elt =
    Js_of_ocaml.Typed_array.set x i elt ;
    x

  let slice x a b = x##slice a b

  let to_bytes x =
    Bytes.of_string (Js_of_ocaml.Typed_array.String.of_uint8Array x)
end

module ESModule = struct
  (* Looks like include Js_object_base does not work here, hence copying *)

  type t = Js.Unsafe.any

  let of_js x = x

  let to_any_js x = x

  let to_string x = Js.to_string (Js.Unsafe.meth_call x "toString" [||])
end

module Memory = struct
  module Buffer = Uint8TypedArray

  let copy_in_buffer buffer src src_offset offset_in_buffer len =
    assert (Number.to_int (Buffer.byte_length buffer) >= offset_in_buffer + len) ;
    let rec aux i =
      if i = len then ()
      else (
        ignore
        @@ Buffer.set
             buffer
             (offset_in_buffer + i)
             (Bytes.get_uint8 src (src_offset + i)) ;
        aux (i + 1) )
    in
    aux 0
end
