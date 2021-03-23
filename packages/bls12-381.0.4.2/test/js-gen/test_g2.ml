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

let () =
  (* let on_resolved m = *)
  let module StubsFr = Bls12_381_js_gen.Fr.MakeStubs (Stubs_node) in
  let module Fr = Bls12_381_gen.Fr.MakeFr (StubsFr) in
  let module StubsG2Uncompressed =
    Bls12_381_js_gen.G2.MakeUncompressedStubs (Stubs_node) in
  let module StubsG2Compressed =
    Bls12_381_js_gen.G2.MakeCompressedStubs (Stubs_node) in
  let module G2Uncompressed =
    Bls12_381_gen.G2.MakeUncompressed (Fr) (StubsG2Uncompressed)
  in
  let module G2Compressed =
    Bls12_381_gen.G2.MakeCompressed (Fr) (StubsG2Compressed)
  in
  let module ValueGeneration = Test_ec_make.MakeValueGeneration (G2Uncompressed) in
  let module IsZero = Test_ec_make.MakeIsZero (G2Uncompressed) in
  let module Equality = Test_ec_make.MakeEquality (G2Uncompressed) in
  let module ECProperties = Test_ec_make.MakeECProperties (G2Uncompressed) in
  let module ValueGenerationCompressed =
    Test_ec_make.MakeValueGeneration (G2Compressed) in
  let module IsZeroCompressed = Test_ec_make.MakeIsZero (G2Compressed) in
  let module EqualityCompressed = Test_ec_make.MakeEquality (G2Compressed) in
  let module ECPropertiesCompressed =
    Test_ec_make.MakeECProperties (G2Compressed) in
  let module Constructors = struct
    let test_of_z_one () =
      (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/fq.rs#L18 *)
      let x =
        ( Z.of_string
            "352701069587466618187139116011060144890029952792775240219908644239793785735715026873347600343865175952761926303160",
          Z.of_string
            "3059144344244213709971259814753781636986470325476647558659373206291635324768958432433509563104347017837885763365758"
        )
      in
      let y =
        ( Z.of_string
            "1985150602287291935568054521177171638300868978215655730859378665066344726373823718423869104263333984641494340347905",
          Z.of_string
            "927553665492332455747201965776037880757740193453592970025027978793976877002675564980949289727957565575433344219582"
        )
      in
      let g = G2Uncompressed.of_z_opt ~x ~y in
      match g with
      | Some g -> assert (G2Uncompressed.eq G2Uncompressed.one g)
      | None -> assert false

    let test_vectors_random_points_not_on_curve () =
      let x = (Z.of_string "90809235435", Z.of_string "09809345809345809") in
      let y =
        (Z.of_string "8090843059809345", Z.of_string "908098039459089345")
      in
      match G2Uncompressed.of_z_opt ~x ~y with
      | Some _ -> assert false
      | None -> assert true

    let get_tests () =
      let open Alcotest in
      ( "From Z elements",
        [ test_case "one (generator)" `Quick test_of_z_one;
          test_case
            "random points not on curve"
            `Quick
            test_vectors_random_points_not_on_curve ] )
  end in
  let open Alcotest in
  run
    "G2 uncompressed"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      Constructors.get_tests ();
      ECProperties.get_tests () ] ;
  run
    "G2 compressed"
    [ IsZeroCompressed.get_tests ();
      ValueGenerationCompressed.get_tests ();
      EqualityCompressed.get_tests ();
      ECPropertiesCompressed.get_tests () ]
