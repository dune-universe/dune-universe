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

module Stubs = Blst_bindings.StubsFq2 (Blst_stubs)

module Fq2 = struct
  exception Not_in_field of Bytes.t

  type t = Blst_bindings.Types.blst_fq2_t Ctypes.ptr

  let size_in_bytes = 96

  let of_bytes_opt bs =
    if Bytes.length bs <> size_in_bytes then None
    else
      let buffer = Blst_bindings.Types.allocate_fq2 () in
      let x_bytes = Bytes.sub bs 0 48 in
      let y_bytes = Bytes.sub bs 48 48 in
      let x_opt = Fq.of_bytes_opt x_bytes in
      let y_opt = Fq.of_bytes_opt y_bytes in
      match (x_opt, y_opt) with
      | (None, _) | (_, None) -> None
      | (Some x, Some y) ->
          Blst_bindings.Types.fq2_assign buffer x y ;
          Some buffer

  let of_bytes_exn bs : t =
    let buffer_opt = of_bytes_opt bs in
    match buffer_opt with
    | None -> raise (Not_in_field bs)
    | Some buffer -> buffer

  let zero =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Blst_bindings.Types.fq2_assign buffer Fq.zero Fq.zero ;
    buffer

  let one =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Blst_bindings.Types.fq2_assign buffer Fq.one Fq.zero ;
    buffer

  let to_bytes p =
    let x = Blst_bindings.Types.fq2_get_x p in
    let y = Blst_bindings.Types.fq2_get_y p in
    let x_bytes = Fq.to_bytes x in
    let y_bytes = Fq.to_bytes y in
    Bytes.concat Bytes.empty [x_bytes; y_bytes]

  let random ?state () =
    (match state with None -> () | Some state -> Random.set_state state) ;
    let x = Fq.random () in
    let y = Fq.random () in
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Blst_bindings.Types.fq2_assign buffer x y ;
    buffer

  let add x y =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Stubs.add buffer x y ;
    buffer

  let ( + ) = add

  let mul x y =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Stubs.mul buffer x y ;
    buffer

  let ( * ) = mul

  let sqrt_opt x =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    let res = Stubs.sqrt buffer x in
    if res then Some buffer else None

  let negate x =
    let buffer = Blst_bindings.Types.allocate_fq2 () in
    Stubs.cneg buffer x true ;
    buffer
end

include Fq2
