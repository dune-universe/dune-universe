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

module MakeUncompressedStubs (M : sig
  val rust_module : unit -> Jsoo_lib.ESModule.t

  val get_wasm_memory_buffer : unit -> Jsoo_lib.Memory.Buffer.t
end) : Bls12_381_gen.G1.RAW_UNCOMPRESSED = struct
  open Js_of_ocaml
  open Js_of_ocaml.Js.Unsafe

  let size_in_bytes = 96

  let check_bytes bs =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      bs
      0
      0
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_uncompressed_check_bytes")
         [| inject 0 |]

  let is_zero bs =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      bs
      0
      0
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_is_zero")
         [| inject 0 |]

  let zero () =
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_zero")
         [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let one () =
    ignore
    @@ fun_call (get (M.rust_module ()) "rustc_bls12_381_g1_one") [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let random () =
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_random")
         [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let add x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      (2 * size_in_bytes)
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_add")
         [| inject 0; inject size_in_bytes; inject (2 * size_in_bytes) |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let mul x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      (2 * size_in_bytes)
      32 ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_mul")
         [| inject 0; inject size_in_bytes; inject (2 * size_in_bytes) |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let eq x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      0
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      size_in_bytes
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_eq")
         [| inject 0; inject size_in_bytes |]

  let negate x =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_negate")
         [| inject 0; inject size_in_bytes |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let double x =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_double")
         [| inject 0; inject size_in_bytes |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let build_from_components x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      48 ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      (size_in_bytes + 48)
      48 ;
    let res =
      Js.to_bool
      @@ fun_call
           (get (M.rust_module ()) "rustc_bls12_381_g1_build_from_components")
           [| inject 0; inject size_in_bytes; inject (size_in_bytes + 48) |]
    in
    if res then
      let res =
        Jsoo_lib.Memory.Buffer.slice
          (M.get_wasm_memory_buffer ())
          0
          size_in_bytes
      in
      Some (Jsoo_lib.Memory.Buffer.to_bytes res)
    else None
end

module MakeCompressedStubs (M : sig
  val rust_module : unit -> Jsoo_lib.ESModule.t

  val get_wasm_memory_buffer : unit -> Jsoo_lib.Memory.Buffer.t
end) : Bls12_381_gen.G1.RAW_COMPRESSED = struct
  open Js_of_ocaml
  open Js_of_ocaml.Js.Unsafe

  let size_in_bytes = 48

  let check_bytes bs =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      bs
      0
      0
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_check_bytes")
         [| inject 0 |]

  let is_zero bs =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      bs
      0
      0
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_is_zero")
         [| inject 0 |]

  let zero () =
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_zero")
         [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let one () =
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_one")
         [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let random () =
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_random")
         [| inject 0 |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let add x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      (2 * size_in_bytes)
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_add")
         [| inject 0; inject size_in_bytes; inject (2 * size_in_bytes) |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let mul x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      (2 * size_in_bytes)
      32 ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_mul")
         [| inject 0; inject size_in_bytes; inject (2 * size_in_bytes) |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let eq x y =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      0
      size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      y
      0
      size_in_bytes
      size_in_bytes ;
    Js.to_bool
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_eq")
         [| inject 0; inject size_in_bytes |]

  let negate x =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_negate")
         [| inject 0; inject size_in_bytes |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let double x =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      x
      0
      size_in_bytes
      size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_g1_compressed_double")
         [| inject 0; inject size_in_bytes |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice (M.get_wasm_memory_buffer ()) 0 size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res
end
