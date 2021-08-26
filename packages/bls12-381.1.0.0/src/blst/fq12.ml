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

module StubsFq12 = Blst_bindings.StubsFq12 (Blst_stubs)

module Fq12 = struct
  exception Not_in_field of Bytes.t

  type t = Blst_bindings.Types.blst_fq12_t Ctypes.ptr

  let order =
    let fq_order =
      Z.of_string
        "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
    in
    Z.pow fq_order 12

  let size_in_bytes = 48 * 12

  let to_bytes p =
    let fill_fp_bytes buffer i bs = Bytes.blit bs 0 buffer i Fq.size_in_bytes in
    let buffer = Bytes.make size_in_bytes '\000' in
    (* Copy paste for readability *)
    let (fq6_x0, fq6_x1) = Blst_bindings.Types.fq12_get_fq_components p in
    let (fq2_x0, fq2_x1, fq2_x2) = fq6_x0 in
    let (fq_x0, fq_x1) = fq2_x0 in
    fill_fp_bytes buffer 0 (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer Fq.size_in_bytes (Fq.to_bytes fq_x1) ;
    let (fq_x0, fq_x1) = fq2_x1 in
    fill_fp_bytes buffer (2 * Fq.size_in_bytes) (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer (3 * Fq.size_in_bytes) (Fq.to_bytes fq_x1) ;
    let (fq_x0, fq_x1) = fq2_x2 in
    fill_fp_bytes buffer (4 * Fq.size_in_bytes) (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer (5 * Fq.size_in_bytes) (Fq.to_bytes fq_x1) ;
    let (fq2_x0, fq2_x1, fq2_x2) = fq6_x1 in
    let (fq_x0, fq_x1) = fq2_x0 in
    fill_fp_bytes buffer (6 * Fq.size_in_bytes) (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer (7 * Fq.size_in_bytes) (Fq.to_bytes fq_x1) ;
    let (fq_x0, fq_x1) = fq2_x1 in
    fill_fp_bytes buffer (8 * Fq.size_in_bytes) (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer (9 * Fq.size_in_bytes) (Fq.to_bytes fq_x1) ;
    let (fq_x0, fq_x1) = fq2_x2 in
    fill_fp_bytes buffer (10 * Fq.size_in_bytes) (Fq.to_bytes fq_x0) ;
    fill_fp_bytes buffer (11 * Fq.size_in_bytes) (Fq.to_bytes fq_x1) ;
    buffer

  let of_bytes_opt bs =
    if Bytes.length bs <> size_in_bytes then None
    else
      let buffer = Blst_bindings.Types.allocate_fq12 () in
      for i = 0 to 11 do
        let bytes = Bytes.sub bs (i * Fq.size_in_bytes) Fq.size_in_bytes in
        let x = Fq.of_bytes_exn bytes in
        Blst_bindings.Types.fq12_assign_fq_component buffer i x
      done ;
      Some buffer

  let random ?state () =
    (match state with None -> () | Some state -> Random.set_state state) ;
    let buffer = Blst_bindings.Types.allocate_fq12 () in
    for i = 0 to 11 do
      let x = Fq.random () in
      Blst_bindings.Types.fq12_assign_fq_component buffer i x
    done ;
    buffer

  let of_bytes_exn bs =
    match of_bytes_opt bs with None -> raise (Not_in_field bs) | Some p -> p

  let one =
    let bs = Bytes.make size_in_bytes '\000' in
    Bytes.set bs 0 '\001' ;
    of_bytes_exn bs

  let zero =
    let bs = Bytes.make size_in_bytes '\000' in
    of_bytes_exn bs

  let eq x y = StubsFq12.equal x y

  let is_zero p = eq p zero

  let is_one p = eq p one

  let mul x y =
    let buffer = Blst_bindings.Types.allocate_fq12 () in
    StubsFq12.mul buffer x y ;
    buffer

  let inverse_opt x =
    if is_zero x then None
    else
      let buffer = Blst_bindings.Types.allocate_fq12 () in
      StubsFq12.inverse buffer x ;
      Some buffer

  let inverse_exn x =
    if is_zero x then raise Division_by_zero
    else
      let buffer = Blst_bindings.Types.allocate_fq12 () in
      StubsFq12.inverse buffer x ;
      buffer

  let of_z x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    let x0 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x0)) in
    let x1 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x1)) in
    let x2 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x2)) in
    let x3 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x3)) in
    let x4 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x4)) in
    let x5 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x5)) in
    let x6 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x6)) in
    let x7 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x7)) in
    let x8 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x8)) in
    let x9 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x9)) in
    let x10 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x10)) in
    let x11 = Fq.of_bytes_exn (Bytes.of_string (Z.to_bits x11)) in
    let buffer = Blst_bindings.Types.allocate_fq12 () in
    Blst_bindings.Types.fq12_assign_fq_component buffer 0 x0 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 1 x1 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 2 x2 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 3 x3 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 4 x4 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 5 x5 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 6 x6 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 7 x7 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 8 x8 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 9 x9 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 10 x10 ;
    Blst_bindings.Types.fq12_assign_fq_component buffer 11 x11 ;
    buffer

  let of_string x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 =
    let x0 = Z.of_string x0 in
    let x1 = Z.of_string x1 in
    let x2 = Z.of_string x2 in
    let x3 = Z.of_string x3 in
    let x4 = Z.of_string x4 in
    let x5 = Z.of_string x5 in
    let x6 = Z.of_string x6 in
    let x7 = Z.of_string x7 in
    let x8 = Z.of_string x8 in
    let x9 = Z.of_string x9 in
    let x10 = Z.of_string x10 in
    let x11 = Z.of_string x11 in
    of_z x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11

  let two_z = Z.(one + one)

  let rec pow x n =
    if Z.equal n Z.zero then one
    else if is_zero x then zero
    else if Z.equal n Z.one then x
    else
      let n = Z.erem n (Z.pred order) in
      let (a, r) = Z.ediv_rem n two_z in
      let acc = pow x a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then acc_square else mul acc_square x
end

include Fq12
