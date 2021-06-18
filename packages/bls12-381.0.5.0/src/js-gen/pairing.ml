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

module MakeStubs (M : sig
  val rust_module : unit -> Jsoo_lib.ESModule.t

  val get_wasm_memory_buffer : unit -> Jsoo_lib.Memory.Buffer.t
end) : Bls12_381_gen.Pairing.RAW_STUBS = struct
  open Js_of_ocaml.Js.Unsafe

  let g1_size_in_bytes = 96

  let g2_size_in_bytes = 192

  let fq12_size_in_bytes = 576

  let miller_loop_simple (g1 : Bytes.t) (g2 : Bytes.t) : Bytes.t =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_simple")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes)
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let pairing (g1 : Bytes.t) (g2 : Bytes.t) : Bytes.t =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes)
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let final_exponentiation (e : Bytes.t) : Bytes.t =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      e
      0
      fq12_size_in_bytes
      fq12_size_in_bytes ;
    ignore
    @@ fun_call
         (get
            (M.rust_module ())
            "rustc_bls12_381_unsafe_pairing_final_exponentiation")
         [| inject 0; inject fq12_size_in_bytes |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let miller_loop_2 g1_1 g1_2 g2_1 g2_2 =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_1
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_2
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes) + g2_size_in_bytes)
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_2")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes);
            inject (fq12_size_in_bytes + (2 * g1_size_in_bytes));
            inject
              (fq12_size_in_bytes + (2 * g1_size_in_bytes) + g2_size_in_bytes)
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let miller_loop_3 g1_1 g1_2 g1_3 g2_1 g2_2 g2_3 =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_3
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_1
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_2
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes) + g2_size_in_bytes)
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_3
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes) + (2 * g2_size_in_bytes))
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_3")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes);
            inject (fq12_size_in_bytes + (2 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (3 * g1_size_in_bytes));
            inject
              (fq12_size_in_bytes + (3 * g1_size_in_bytes) + g2_size_in_bytes);
            inject
              ( fq12_size_in_bytes + (3 * g1_size_in_bytes)
              + (2 * g2_size_in_bytes) )
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let miller_loop_4 g1_1 g1_2 g1_3 g1_4 g2_1 g2_2 g2_3 g2_4 =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_3
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_4
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_1
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_2
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes) + g2_size_in_bytes)
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_3
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes) + (2 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_4
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes) + (3 * g2_size_in_bytes))
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_4")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes);
            inject (fq12_size_in_bytes + (2 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (3 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (4 * g1_size_in_bytes));
            inject
              (fq12_size_in_bytes + (4 * g1_size_in_bytes) + g2_size_in_bytes);
            inject
              ( fq12_size_in_bytes + (4 * g1_size_in_bytes)
              + (2 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (4 * g1_size_in_bytes)
              + (3 * g2_size_in_bytes) )
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let miller_loop_5 g1_1 g1_2 g1_3 g1_4 g1_5 g2_1 g2_2 g2_3 g2_4 g2_5 =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_3
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_4
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_5
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_1
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_2
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes) + g2_size_in_bytes)
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_3
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes) + (2 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_4
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes) + (3 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_5
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes) + (4 * g2_size_in_bytes))
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_5")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes);
            inject (fq12_size_in_bytes + (2 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (3 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (4 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (5 * g1_size_in_bytes));
            inject
              (fq12_size_in_bytes + (5 * g1_size_in_bytes) + g2_size_in_bytes);
            inject
              ( fq12_size_in_bytes + (5 * g1_size_in_bytes)
              + (2 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (5 * g1_size_in_bytes)
              + (3 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (5 * g1_size_in_bytes)
              + (4 * g2_size_in_bytes) )
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res

  let miller_loop_6 g1_1 g1_2 g1_3 g1_4 g1_5 g1_6 g2_1 g2_2 g2_3 g2_4 g2_5 g2_6
      =
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_1
      0
      fq12_size_in_bytes
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_2
      0
      (fq12_size_in_bytes + g1_size_in_bytes)
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_3
      0
      (fq12_size_in_bytes + (2 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_4
      0
      (fq12_size_in_bytes + (3 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_5
      0
      (fq12_size_in_bytes + (4 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g1_6
      0
      (fq12_size_in_bytes + (5 * g1_size_in_bytes))
      g1_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_1
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_2
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes) + g2_size_in_bytes)
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_3
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes) + (2 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_4
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes) + (3 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_5
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes) + (4 * g2_size_in_bytes))
      g2_size_in_bytes ;
    Jsoo_lib.Memory.copy_in_buffer
      (M.get_wasm_memory_buffer ())
      g2_6
      0
      (fq12_size_in_bytes + (6 * g1_size_in_bytes) + (5 * g2_size_in_bytes))
      g2_size_in_bytes ;
    ignore
    @@ fun_call
         (get (M.rust_module ()) "rustc_bls12_381_pairing_miller_loop_6")
         [| inject 0;
            inject fq12_size_in_bytes;
            inject (fq12_size_in_bytes + g1_size_in_bytes);
            inject (fq12_size_in_bytes + (2 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (3 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (4 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (5 * g1_size_in_bytes));
            inject (fq12_size_in_bytes + (6 * g1_size_in_bytes));
            inject
              (fq12_size_in_bytes + (6 * g1_size_in_bytes) + g2_size_in_bytes);
            inject
              ( fq12_size_in_bytes + (6 * g1_size_in_bytes)
              + (2 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (6 * g1_size_in_bytes)
              + (3 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (6 * g1_size_in_bytes)
              + (4 * g2_size_in_bytes) );
            inject
              ( fq12_size_in_bytes + (6 * g1_size_in_bytes)
              + (5 * g2_size_in_bytes) )
         |] ;
    let res =
      Jsoo_lib.Memory.Buffer.slice
        (M.get_wasm_memory_buffer ())
        0
        fq12_size_in_bytes
    in
    Jsoo_lib.Memory.Buffer.to_bytes res
end
