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

open Core_bench.Std

module ECBenchmark (G : Bls12_381_gen.Elliptic_curve_sig.T) = struct
  let g1 = G.random ()

  let g2 = G.random ()

  let one = G.one

  let zero = G.zero

  let scalar = G.Scalar.random ()

  let g1_bytes = G.to_bytes g1

  let g2_bytes = G.to_bytes g1

  let zero_bytes = G.to_bytes zero

  let one_bytes = G.to_bytes one

  let generate_random_element () = ignore @@ G.random ()

  let generate_zero () = ignore @@ G.zero

  let generate_one () = ignore @@ G.one

  let check_of_bytes_exn_on_pregenerated_random_element () =
    ignore @@ G.of_bytes_exn g1_bytes

  let check_of_bytes_exn_on_pregenerated_zero_element () =
    ignore @@ G.of_bytes_exn zero_bytes

  let check_of_bytes_exn_on_pregenerated_one_element () =
    ignore @@ G.of_bytes_exn one_bytes

  let check_is_zero_on_pregenerated_random () = ignore @@ G.is_zero g1

  let check_is_zero_on_pregenerated_one () = ignore @@ G.is_zero one

  let check_is_zero_on_pregenerated_zero () = ignore @@ G.is_zero zero

  let compute_addition_pregenerated_random () = G.add g1 g2

  let check_equality_on_random () = ignore @@ G.eq g1 g2

  let check_equality_on_one_and_random () = ignore @@ G.eq g1 one

  let check_equality_on_zero_and_random () = ignore @@ G.eq g1 zero

  let check_equality_on_same_element () = ignore @@ G.eq g1 g1

  let compute_mul_on_pregenerated_random_element_and_scalar () =
    ignore @@ G.mul g1 scalar

  let compute_opposite_of_pregenerated_random_element () = ignore @@ G.negate g1

  let compute_opposite_of_zero () = ignore @@ G.negate zero

  let compute_opposite_of_one () = ignore @@ G.negate one

  let get_benches ec_name =
    [ Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute addition pregenerated random element"
             ec_name)
        compute_addition_pregenerated_random;
      Bench.Test.create
        ~name:(Printf.sprintf "%s random generation" ec_name)
        generate_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s zero generation" ec_name)
        generate_zero;
      Bench.Test.create
        ~name:(Printf.sprintf "%s one generation" ec_name)
        generate_one;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s check of_bytes_exn on pregenerated random"
             ec_name)
        check_of_bytes_exn_on_pregenerated_random_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf "%s check of_bytes_exn on pregenerated zero" ec_name)
        check_of_bytes_exn_on_pregenerated_zero_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf "%s check of_bytes_exn on pregenerated one" ec_name)
        check_of_bytes_exn_on_pregenerated_one_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated random" ec_name)
        check_is_zero_on_pregenerated_random;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated one" ec_name)
        check_is_zero_on_pregenerated_one;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check if zero on pregenerated zero" ec_name)
        check_is_zero_on_pregenerated_zero;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check equality on random" ec_name)
        check_equality_on_random;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check equality on one and random" ec_name)
        check_equality_on_one_and_random;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check equality on zero and random" ec_name)
        check_equality_on_zero_and_random;
      Bench.Test.create
        ~name:(Printf.sprintf "%s check equality on same element" ec_name)
        check_equality_on_same_element;
      Bench.Test.create
        ~name:
          (Printf.sprintf
             "%s compute scalar multiplication on pregenerated random element \
              and scalar"
             ec_name)
        compute_mul_on_pregenerated_random_element_and_scalar;
      Bench.Test.create
        ~name:
          (Printf.sprintf "%s opposite of pregenerated random element" ec_name)
        compute_opposite_of_pregenerated_random_element;
      Bench.Test.create
        ~name:(Printf.sprintf "%s opposite of zero" ec_name)
        compute_opposite_of_zero;
      Bench.Test.create
        ~name:(Printf.sprintf "%s opposite of one" ec_name)
        compute_opposite_of_one ]
end

module BenchmarkG1Uncompressed = ECBenchmark (Bls12_381.G1.Uncompressed)
module BenchmarkG1Compressed = ECBenchmark (Bls12_381.G1.Compressed)
module BenchmarkG2Uncompressed = ECBenchmark (Bls12_381.G2.Uncompressed)
module BenchmarkG2Compressed = ECBenchmark (Bls12_381.G2.Compressed)

let () =
  let commands =
    List.concat
      [ BenchmarkG1Uncompressed.get_benches "G1 Uncompressed";
        BenchmarkG1Compressed.get_benches "G1 Compressed";
        BenchmarkG2Uncompressed.get_benches "G2 Uncompressed";
        BenchmarkG2Compressed.get_benches "G2 Compressed" ]
  in
  Core.Command.run (Bench.make_command commands)
