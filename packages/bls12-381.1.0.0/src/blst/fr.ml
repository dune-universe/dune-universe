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

module Stubs = Blst_bindings.StubsFr (Blst_stubs)

module Fr = struct
  exception Not_in_field of Bytes.t

  type t = Blst_bindings.Types.blst_fr_t Ctypes.ptr

  let size_in_bytes = 32

  let order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"

  let pad_if_require bs =
    (* Pad to 32 bytes. In anycase, copy the bytes to a new buffer *)
    if Bytes.length bs < size_in_bytes then (
      let padded_bytes = Bytes.make size_in_bytes '\000' in
      Bytes.blit bs 0 padded_bytes 0 (Bytes.length bs) ;
      padded_bytes )
    else Bytes.copy bs

  let of_bytes_opt bs =
    if Bytes.length bs > size_in_bytes then None
    else
      let bs = pad_if_require bs in
      let buffer_scalar = Blst_bindings.Types.allocate_scalar () in
      let () =
        Stubs.scalar_of_bytes_le buffer_scalar (Ctypes.ocaml_bytes_start bs)
      in
      if Stubs.check_scalar buffer_scalar then (
        let buffer_fr = Blst_bindings.Types.allocate_fr () in
        Stubs.fr_of_scalar buffer_fr buffer_scalar ;
        Some buffer_fr )
      else None

  let of_bytes_exn bs =
    let buffer_opt = of_bytes_opt bs in
    match buffer_opt with
    | None -> raise (Not_in_field bs)
    | Some buffer -> buffer

  let check_bytes bs =
    if Bytes.length bs = size_in_bytes then (
      let buffer_scalar = Blst_bindings.Types.allocate_scalar () in
      Stubs.scalar_of_bytes_le buffer_scalar (Ctypes.ocaml_bytes_start bs) ;
      Stubs.check_scalar buffer_scalar )
    else false

  let zero = of_bytes_exn (Bytes.make size_in_bytes '\000')

  let one =
    let bytes = Bytes.make size_in_bytes '\000' in
    Bytes.set bytes 0 '\001' ;
    of_bytes_exn bytes

  let to_bytes x =
    let buffer_bytes = Bytes.make size_in_bytes '\000' in
    let buffer_scalar = Blst_bindings.Types.allocate_scalar () in
    Stubs.scalar_of_fr buffer_scalar x ;
    Stubs.scalar_to_bytes_le
      (Ctypes.ocaml_bytes_start buffer_bytes)
      buffer_scalar ;
    buffer_bytes

  let eq x y =
    let x_bytes = to_bytes x in
    let y_bytes = to_bytes y in
    Bytes.equal x_bytes y_bytes

  let ( = ) = eq

  let is_zero s = eq s zero

  let is_one s = eq s one

  let rec random ?state () =
    (match state with None -> () | Some state -> Random.set_state state) ;
    let random_bytes =
      Bytes.init size_in_bytes (fun _ -> char_of_int @@ Random.int 256)
    in
    let res = of_bytes_opt random_bytes in
    match res with None -> random ?state:None () | Some res -> res

  let rec non_null_random ?state () =
    let r = random ?state () in
    if is_zero r then non_null_random ?state () else r

  let add x y =
    let buffer = Blst_bindings.Types.allocate_fr () in
    Stubs.add buffer x y ;
    buffer

  let ( + ) = add

  let mul x y =
    let buffer = Blst_bindings.Types.allocate_fr () in
    Stubs.mul buffer x y ;
    buffer

  let ( * ) = mul

  let inverse_opt x =
    if is_zero x then None
    else
      let buffer = Blst_bindings.Types.allocate_fr () in
      Stubs.eucl_inverse buffer x ;
      Some buffer

  let inverse_exn x =
    match inverse_opt x with None -> raise Division_by_zero | Some x -> x

  let sub a b =
    let buffer = Blst_bindings.Types.allocate_fr () in
    Stubs.sub buffer a b ;
    buffer

  let square x = x * x

  let double x = x + x

  let negate x = sub zero x

  let ( - ) = negate

  let div_exn x y = x * inverse_exn y

  let div_opt x y =
    match inverse_opt y with None -> None | Some inv_y -> Some (x * inv_y)

  let ( / ) = div_exn

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

  let ( ** ) = pow

  let to_string s =
    let bytes = to_bytes s in
    let z = Z.of_bits (Bytes.to_string bytes) in
    Z.to_string z

  let of_z z =
    let z = Bytes.of_string (Z.to_bits (Z.erem z order)) in
    let x = Bytes.make size_in_bytes '\000' in
    Bytes.blit z 0 x 0 (min (Bytes.length z) size_in_bytes) ;
    of_bytes_exn x

  let to_z b =
    let bytes = to_bytes b in
    Z.of_bits (Bytes.to_string bytes)

  let of_string s = of_z (Z.of_string s)

  let factor_power_of_two =
    let rec aux i n =
      let (q, r) = Z.ediv_rem n two_z in
      if Z.equal r Z.zero then aux Int.(succ i) q else (i, n)
    in
    aux 0 (Z.pred order)

  let legendre_symbol x =
    if is_zero x then Z.zero
    else if is_one (pow x (Z.divexact (Z.pred order) (Z.of_int 2))) then Z.one
    else Z.neg Z.one

  let is_quadratic_residue x =
    if is_zero x then true else Z.equal (legendre_symbol x) Z.one

  let rec pick_non_square () =
    let z = random () in
    if Z.equal (legendre_symbol z) (Z.of_int (-1)) then z
    else pick_non_square ()

  let sqrt_opt x =
    if not (is_quadratic_residue x) then None
    else
      (* https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm *)
      let (s, q) = factor_power_of_two in
      (* implies p = 3 mod 4 *)
      if Int.equal s 1 then
        (* r = x^((p + 1) / 4) *)
        let r = pow x (Z.divexact (Z.succ order) (Z.of_string "4")) in
        Some r
      else
        let rec compute_lowest_n_2th_root_of_unity (i : int) x upper : int =
          let x = square x in
          if is_one x then i
          else if Int.(equal i upper) then
            failwith "Upperbound should be higher"
            (* should never happen in this case, just being explicit *)
          else compute_lowest_n_2th_root_of_unity (Int.succ i) x upper
        in
        let z = pick_non_square () in
        let c = pow z q in
        let rec aux m c t r =
          if eq t zero then zero (* case x is zero *)
          else if eq t one then r (* base case *)
          else
            let i = compute_lowest_n_2th_root_of_unity 1 t m in
            let b = pow c (Z.pow two_z Int.(pred (sub m i))) in
            let m = i in
            let c = mul b b in
            let t = mul t c in
            let r = mul r b in
            aux m c t r
        in
        Some (aux s c (pow x q) (pow x (Z.divexact (Z.succ q) two_z)))

  let fft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = inverse_exn

      let scalar_of_z = of_z
    end in
    Fft.fft (module M) ~domain ~points

  let ifft ~domain ~points =
    let module M = struct
      type group = t

      type scalar = t

      let zero = zero

      let mul = mul

      let add = add

      let sub x y = add x (negate y)

      let inverse_exn_scalar = inverse_exn

      let scalar_of_z = of_z
    end in
    Fft.ifft (module M) ~domain ~points
end

include Fr
