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

module type RAW_BASE = sig
  val order : Z.t

  val size_in_bytes : int

  val check_bytes : Bytes.t -> bool

  val is_zero : Bytes.t -> bool

  val is_one : Bytes.t -> bool

  val random : unit -> Bytes.t

  val zero : unit -> Bytes.t

  val one : unit -> Bytes.t

  val add : Bytes.t -> Bytes.t -> Bytes.t

  val mul : Bytes.t -> Bytes.t -> Bytes.t

  val unsafe_inverse : Bytes.t -> Bytes.t

  val eq : Bytes.t -> Bytes.t -> bool

  val negate : Bytes.t -> Bytes.t

  val square : Bytes.t -> Bytes.t

  val double : Bytes.t -> Bytes.t

  val pow : Bytes.t -> Bytes.t -> Bytes.t
end

module Make (Stubs : RAW_BASE) : Ff_sig.BASE = struct
  type t = Bytes.t

  exception Not_in_field of Bytes.t

  let order = Stubs.order

  let size_in_bytes = Stubs.size_in_bytes

  let pad_if_require bs =
    (* Pad to 32 bytes. In anycase, copy the bytes to a new buffer *)
    if Bytes.length bs < size_in_bytes then (
      let padded_bytes = Bytes.make size_in_bytes '\000' in
      Bytes.blit bs 0 padded_bytes 0 (Bytes.length bs) ;
      padded_bytes )
    else Bytes.copy bs

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then Stubs.check_bytes bs else false

  let of_bytes_opt bs =
    let bs = pad_if_require bs in
    if check_bytes bs then Some bs else None

  let of_bytes_exn (g : Bytes.t) : t =
    let g = pad_if_require g in
    if check_bytes g then g else raise (Not_in_field g)

  let to_bytes g = g

  let is_zero g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    Stubs.is_zero g

  let is_one g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    Stubs.is_one g

  let zero =
    let g = Stubs.zero () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let one =
    let g = Stubs.one () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let random ?state () =
    ignore state ;
    let g = Stubs.random () in
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    g

  let rec non_null_random ?state () =
    ignore state ;
    let r = random () in
    if is_zero r then non_null_random () else r

  let add x y =
    assert (Bytes.length x = Stubs.size_in_bytes) ;
    assert (Bytes.length y = Stubs.size_in_bytes) ;
    let res = Stubs.add x y in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( + ) = add

  let mul x y =
    assert (Bytes.length x = Stubs.size_in_bytes) ;
    assert (Bytes.length y = Stubs.size_in_bytes) ;
    let res = Stubs.mul x y in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( * ) = mul

  let inverse_exn g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.unsafe_inverse g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let inverse_opt g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    if is_zero g then None
    else
      let res = Stubs.unsafe_inverse g in
      assert (Bytes.length res = Stubs.size_in_bytes) ;
      Some res

  let negate g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.negate g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let ( - ) = negate

  let sub a b = add a (negate b)

  let square g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.square g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let double g =
    assert (Bytes.length g = Stubs.size_in_bytes) ;
    let res = Stubs.double g in
    assert (Bytes.length res = Stubs.size_in_bytes) ;
    res

  let eq x y = Stubs.eq x y

  let ( = ) = eq

  let pow x n =
    let n = Z.erem n (Z.pred Stubs.order) in
    (* sign is removed by to_bits, but that's fine because we used mod before *)
    let n = Bytes.of_string (Z.to_bits n) in
    let bytes_size_n = Bytes.length n in
    let padded_n =
      Bytes.init Stubs.size_in_bytes (fun i ->
          if i < bytes_size_n then Bytes.get n i else char_of_int 0)
    in
    let res = Stubs.pow (to_bytes x) padded_n in
    (* assert (Bytes.length res = Stubs.size_in_bytes) ; *)
    res

  let ( ** ) = pow

  let div_exn a b =
    if b = zero then raise Division_by_zero else mul a (inverse_exn b)

  let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

  let ( / ) = div_exn
end
