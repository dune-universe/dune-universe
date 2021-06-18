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

module G2 = Bls12_381.G2

module FFT = struct
  let rec power2 x = if x = 0 then 1 else 2 * power2 (x - 1)

  (* Output the domain comprising the powers of the root of unity*)
  let generate_domain power size is_inverse =
    let omega_base =
      Bls12_381.Fr.of_string
        "0x16a2a19edfe81f20d09b681922c813b4b63683508c2280b93829971f439f0d2b"
    in
    let rec get_omega limit is_inverse =
      if limit < 32 then Bls12_381.Fr.square (get_omega (limit + 1) is_inverse)
      else if is_inverse = false then omega_base
      else Bls12_381.Fr.inverse_exn omega_base
    in
    let omega = get_omega power is_inverse in
    Array.init size (fun i -> Bls12_381.Fr.pow omega (Z.of_int i))

  let parse_group_elements_from_file n f =
    let ic = open_in_bin f in
    let group_elements =
      List.init n (fun _i ->
          let bytes_buf = Bytes.create G2.size_in_bytes in
          Stdlib.really_input ic bytes_buf 0 G2.size_in_bytes ;
          G2.of_bytes_exn bytes_buf)
    in
    close_in ic ;
    group_elements

  let test_fft () =
    let power = 2 in
    let m = power2 power in
    let omega_domain = generate_domain power m false in
    let g2_elements = parse_group_elements_from_file m "test_vector_g2_2" in
    let result = G2.fft ~domain:omega_domain ~points:g2_elements in
    let expected_result =
      parse_group_elements_from_file m "fft_test_vector_g2_2"
    in
    assert (result = expected_result)

  let test_ifft () =
    let power = 2 in
    let m = power2 power in
    let omega_domain = generate_domain power m true in
    let g2_elements = parse_group_elements_from_file m "test_vector_g2_2" in
    let result = G2.ifft ~domain:omega_domain ~points:g2_elements in
    let expected_result =
      parse_group_elements_from_file m "ifft_test_vector_g2_2"
    in
    assert (result = expected_result)

  let get_tests () =
    let open Alcotest in
    ( "(i)FFT of G2 uncompressed",
      [test_case "fft" `Quick test_fft; test_case "ifft" `Quick test_ifft] )
end

let () =
  let open Alcotest in
  run "G2 FFT Uncompressed" [FFT.get_tests ()]
