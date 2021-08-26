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

module Stubs = Blst_bindings.StubsG1 (Blst_stubs)

module G1 = struct
  exception Not_on_curve of Bytes.t

  type t = Blst_bindings.Types.blst_g1_t Ctypes.ptr

  let size_in_bytes = 96

  module Scalar = Fr

  let cofactor_fr = Scalar.of_string "76329603384216526031706109802092473003"

  let empty () = Blst_bindings.Types.allocate_g1 ()

  let check_bytes bs =
    let buffer = Blst_bindings.Types.allocate_g1_affine () in
    Stubs.deserialize buffer (Ctypes.ocaml_bytes_start bs) = 0

  let of_bytes_opt bs =
    let buffer_affine = Blst_bindings.Types.allocate_g1_affine () in
    if Bytes.length bs <> size_in_bytes then None
    else
      let res = Stubs.deserialize buffer_affine (Ctypes.ocaml_bytes_start bs) in
      if res = 0 then (
        let buffer = Blst_bindings.Types.allocate_g1 () in
        Stubs.from_affine buffer buffer_affine ;
        let is_in_prime_subgroup = Stubs.in_g1 buffer in
        if is_in_prime_subgroup then Some buffer else None )
      else None

  let of_bytes_exn bs =
    match of_bytes_opt bs with None -> raise (Not_on_curve bs) | Some p -> p

  let zero =
    let bytes =
      Bytes.of_string
        "@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
    in
    of_bytes_exn bytes

  let one =
    let bytes =
      Bytes.of_string
        "\023\241\211\1671\151\215\148&\149c\140O\169\172\015\195h\140O\151t\185\005\161N:?\023\027\172XlU\232?\249z\026\239\251:\240\n\
         \219\"\198\187\b\179\244\129\227\170\160\241\160\1580\237t\029\138\228\252\245\224\149\213\208\n\
         \246\000\219\024\203,\004\179\237\208<\199D\162\136\138\228\012\170#)F\197\231\225"
    in
    of_bytes_exn bytes

  let of_compressed_bytes_opt bs =
    let buffer_affine = Blst_bindings.Types.allocate_g1_affine () in
    let res = Stubs.uncompress buffer_affine (Ctypes.ocaml_bytes_start bs) in
    if res = 0 then (
      let buffer = Blst_bindings.Types.allocate_g1 () in
      Stubs.from_affine buffer buffer_affine ;
      let is_in_prime_subgroup = Stubs.in_g1 buffer in
      if is_in_prime_subgroup then Some buffer else None )
    else None

  let of_compressed_bytes_exn bs =
    match of_compressed_bytes_opt bs with
    | None -> raise (Not_on_curve bs)
    | Some p -> p

  let to_bytes p =
    let buffer = Bytes.make size_in_bytes '\000' in
    Stubs.serialize (Ctypes.ocaml_bytes_start buffer) p ;
    buffer

  let to_compressed_bytes p =
    let buffer = Bytes.make (size_in_bytes / 2) '\000' in
    Stubs.compress (Ctypes.ocaml_bytes_start buffer) p ;
    buffer

  let add x y =
    (* dadd must be used to be complete. add does not work when it is the same
       point
    *)
    let buffer = Blst_bindings.Types.allocate_g1 () in
    Stubs.dadd buffer x y ;
    buffer

  let double x =
    let buffer = Blst_bindings.Types.allocate_g1 () in
    Stubs.double buffer x ;
    buffer

  let mul g n =
    let bytes = Fr.to_bytes n in
    let buffer = Blst_bindings.Types.allocate_g1 () in
    Stubs.mult
      buffer
      g
      (Ctypes.ocaml_bytes_start bytes)
      (Unsigned.Size_t.of_int (32 * 8)) ;
    buffer

  let b = Fq.(one + one + one + one)

  let rec random ?state () =
    (match state with None -> () | Some state -> Random.set_state state) ;
    let x = Fq.random () in
    let xx = Fq.(x * x) in
    let xxx = Fq.(x * xx) in
    let xxx_plus_b = Fq.(xxx + b) in
    let y_opt = Fq.sqrt_opt xxx_plus_b in
    match y_opt with
    | None -> random ()
    | Some y ->
        let y = if Random.bool () then y else Fq.negate y in
        let p_affine = Blst_bindings.Types.allocate_g1_affine () in
        Blst_bindings.Types.g1_affine_set_x p_affine x ;
        Blst_bindings.Types.g1_affine_set_y p_affine y ;
        let p = Blst_bindings.Types.allocate_g1 () in
        Stubs.from_affine p p_affine ;
        mul p cofactor_fr

  let eq g1 g2 = Stubs.equal g1 g2

  let is_zero x = eq x zero

  let order_minus_one = Scalar.(negate one)

  let negate g =
    let buffer = Blst_bindings.Types.g1_copy g in
    Stubs.cneg buffer true ;
    buffer

  let of_z_opt ~x ~y =
    let x_bytes = Bytes.of_string (Z.to_bits (Z.erem x Fq.order)) in
    let x_bytes_le = Bytes.make (size_in_bytes / 2) '\000' in
    Bytes.blit
      x_bytes
      0
      x_bytes_le
      0
      (min (Bytes.length x_bytes) (size_in_bytes / 2)) ;
    let x_bytes_be =
      Bytes.init (size_in_bytes / 2) (fun i ->
          Bytes.get x_bytes_le ((size_in_bytes / 2) - i - 1))
    in
    let y_bytes = Bytes.of_string (Z.to_bits (Z.erem y Fq.order)) in
    let y_bytes_le = Bytes.make (size_in_bytes / 2) '\000' in
    Bytes.blit
      y_bytes
      0
      y_bytes_le
      0
      (min (Bytes.length y_bytes) (size_in_bytes / 2)) ;
    let y_bytes_be =
      Bytes.init (size_in_bytes / 2) (fun i ->
          Bytes.get y_bytes_le ((size_in_bytes / 2) - i - 1))
    in
    let b = Bytes.concat Bytes.empty [x_bytes_be; y_bytes_be] in
    of_bytes_opt b

  let fft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = Scalar.t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = Scalar.inverse_exn

      let scalar_of_z = Scalar.of_z
    end in
    Fft.fft (module M) ~domain ~points

  let ifft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = Scalar.t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = Scalar.inverse_exn

      let scalar_of_z = Scalar.of_z
    end in
    Fft.ifft (module M) ~domain ~points

  let hash_to_curve message dst =
    let message_length = Bytes.length message in
    let dst_length = Bytes.length dst in
    let buffer = Blst_bindings.Types.allocate_g1 () in
    Stubs.hash_to_curve
      buffer
      (Ctypes.ocaml_bytes_start message)
      (Unsigned.Size_t.of_int message_length)
      (Ctypes.ocaml_bytes_start dst)
      (Unsigned.Size_t.of_int dst_length)
      (Ctypes.ocaml_bytes_start Bytes.empty)
      Unsigned.Size_t.zero ;
    buffer
end

include G1
