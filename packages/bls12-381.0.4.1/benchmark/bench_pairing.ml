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

open Core_bench

let g1 = Bls12_381.G1.Uncompressed.random ()

let g2 = Bls12_381.G2.Uncompressed.random ()

let three_g1_and_g2 =
  [ (Bls12_381.G1.Uncompressed.random (), Bls12_381.G2.Uncompressed.random ());
    (Bls12_381.G1.Uncompressed.random (), Bls12_381.G2.Uncompressed.random ());
    (Bls12_381.G1.Uncompressed.random (), Bls12_381.G2.Uncompressed.random ())
  ]

let a = Bls12_381.Fq12.random ()

let compute_pairing_on_pregenerated_random_elements () =
  Bls12_381.Pairing.pairing g1 g2

let compute_miller_loop_on_pregenerated_random_elements () =
  Bls12_381.Pairing.miller_loop_simple g1 g2

let compute_final_exponentiation_on_pregenerated_random_element () =
  Bls12_381.Pairing.final_exponentiation_opt a

let compute_miller_loop_on_three_pregenerated_couple_of_uncompressed_random_elements
    () =
  Bls12_381.Pairing.miller_loop three_g1_and_g2

let compute_miller_loop_on_three_pregenerated_couple_of_uncompressed_random_elements_followed_by_final_exponentiation
    () =
  Bls12_381.Pairing.final_exponentiation_opt
  @@ Bls12_381.Pairing.miller_loop three_g1_and_g2

let () =
  Core.Command.run
    (Bench.make_command
       [ Bench.Test.create
           ~name:"Pairing on pregenerated uncompressed random elements"
           compute_pairing_on_pregenerated_random_elements;
         Bench.Test.create
           ~name:"Miller loop on pregenerated uncompressed random elements"
           compute_miller_loop_on_pregenerated_random_elements;
         Bench.Test.create
           ~name:
             "Miller loop on three pregenerated couples of uncompressed random \
              elements"
           compute_miller_loop_on_three_pregenerated_couple_of_uncompressed_random_elements;
         Bench.Test.create
           ~name:
             "Miller loop on three pregenerated couples of uncompressed random \
              elements followed by final exponentiation"
           compute_miller_loop_on_three_pregenerated_couple_of_uncompressed_random_elements_followed_by_final_exponentiation;
         Bench.Test.create
           ~name:"Final exponentiation on pregenerated random element"
           compute_final_exponentiation_on_pregenerated_random_element ])
