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
module ValueGeneration = Test_ec_make.MakeValueGeneration (G2)
module IsZero = Test_ec_make.MakeIsZero (G2)
module Equality = Test_ec_make.MakeEquality (G2)
module ECProperties = Test_ec_make.MakeECProperties (G2)

module Constructors = struct
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
    let g = G2.of_z_opt ~x ~y in
    match g with Some g -> assert (G2.eq G2.one g) | None -> assert false

  let test_vectors_random_points_not_on_curve () =
    let x = (Z.of_string "90809235435", Z.of_string "09809345809345809") in
    let y =
      (Z.of_string "8090843059809345", Z.of_string "908098039459089345")
    in
    match G2.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let get_tests () =
    let open Alcotest in
    ( "From Z elements",
      [ test_case "one (generator)" `Quick test_of_z_one;
        test_case
          "random points not on curve"
          `Quick
          test_vectors_random_points_not_on_curve ] )
end

module UncompressedRepresentation = struct
  let test_uncompressed_zero_has_first_byte_at_64 () =
    assert (int_of_char (Bytes.get G2.(to_bytes zero) 0) = 64)

  let test_uncompressed_random_has_first_byte_strictly_lower_than_64 () =
    assert (int_of_char (Bytes.get G2.(to_bytes (random ())) 0) < 64)

  let get_tests () =
    let open Alcotest in
    ( "Representation of G2 Uncompressed",
      [ test_case
          "zero has first byte at 64"
          `Quick
          test_uncompressed_zero_has_first_byte_at_64;
        test_case
          "random has first byte strictly lower than 64"
          `Quick
          (Test_ec_make.repeat
             1000
             test_uncompressed_random_has_first_byte_strictly_lower_than_64) ]
    )
end

module CompressedRepresentation = struct
  include Test_ec_make.MakeCompressedRepresentation (G2)

  let test_vectors () =
    let vectors =
      [ ( Hex.to_bytes
            (`Hex
              "93e02b6052719f607dacd3a088274f65596bd0d09920b61ab5da61bbdc7f5049334cf11213945d57e5ac7d055d042b7e024aa2b2f08f0a91260805272dc51051c6e47ad4fa403b02b4510b647ae3d1770bac0326a805bbefd48056c8c121bdb8"),
          G2.to_compressed_bytes G2.one );
        ( Hex.to_bytes
            (`Hex
              "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
          G2.to_compressed_bytes G2.zero ) ]
    in
    List.iter
      (fun (expected_bytes, computed_bytes) ->
        assert (Bytes.equal expected_bytes computed_bytes))
      vectors

  (* Random points generated for regression tests
     ```ocaml
     let () =
       let x = Bls12_381.G2.random () in
       Printf.printf
         "Compressed: %s\nUncompressed: %s\n"
         Hex.(show (of_bytes (Bls12_381.G2.to_compressed_bytes x)))
         Hex.(show (of_bytes (Bls12_381.G2.to_bytes x)))
     ```
  *)
  let regression_tests () =
    let vectors =
      [ ( G2.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "03b7462a980d8d0f71b8adb247bff135340c2732780a31c2961c23ed7532779d72a4f5510754285f6a0f6eac059fed0b0f612738ca78fc16d43a68e53a27a7f43e1f1e2f2720c160af67d2636a799d5a8bf4d8654601543992ad1c0f7293249304c27382c74e088848b3ad156a3ca03f7c4841621710ae09a3c462ad2bf783dcf279ff1836b52837f38be885b59df960109dc5aad7bb0163f11f740e7502377a581d8d02ee209e6e554bffe41b1913b1a6a7f1a72dac9bd63d4448ddc0a3a5af"),
          Hex.to_bytes
            (`Hex
              "83b7462a980d8d0f71b8adb247bff135340c2732780a31c2961c23ed7532779d72a4f5510754285f6a0f6eac059fed0b0f612738ca78fc16d43a68e53a27a7f43e1f1e2f2720c160af67d2636a799d5a8bf4d8654601543992ad1c0f72932493")
        );
        ( G2.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "17d24507e389a55fa1816bf1ee6b8310b199198d5e43e2e35b1f9e5c546a8e0ac9bd50a6d72a9a27f80023db8db3d6bb08f4787f10c214bc86e8fb9ce079199fc56a4208c3f1e954aef4c3cee810b394ef69ddef9ce965f513995a124fa6286d0341b659d2cf349691758dbf296bc585eb155e73ca21285a293a588556dfadbef2359a3973b49f76b8217f2c49aad91212d2a5ca19f5a95cef69ec782a44767db7dbd0a44fbc2ff2190fc69340aaf59495ad969f6c07288a5ddcfe742681890a"),
          Hex.to_bytes
            (`Hex
              "97d24507e389a55fa1816bf1ee6b8310b199198d5e43e2e35b1f9e5c546a8e0ac9bd50a6d72a9a27f80023db8db3d6bb08f4787f10c214bc86e8fb9ce079199fc56a4208c3f1e954aef4c3cee810b394ef69ddef9ce965f513995a124fa6286d")
        );
        ( G2.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "12c37d6040d2e2ce9813dd48fdf6abd1754506aa2bfb396c2be7cfabf4375b7ec3d6054d855bc3f93472ac8e8e6ac1280e063837200a2eb0219ecbda76b07b81620f42b524c2d054b1c56e3362a245d8f45cbaf863fc3c01a2cfa20eb4ffe87c151325e3b39c45eef19e7799dcd5edff8ab746ded19983843c9005bd7defd1263d2211ad641f3eb45cf9c6764bcfabca071e2f6b77819c554b6ef9e26a5e23bbb1a83e448afec180f5fcb11d6c740badf015b68a64d80036202791ef19f4e7fb"),
          Hex.to_bytes
            (`Hex
              "b2c37d6040d2e2ce9813dd48fdf6abd1754506aa2bfb396c2be7cfabf4375b7ec3d6054d855bc3f93472ac8e8e6ac1280e063837200a2eb0219ecbda76b07b81620f42b524c2d054b1c56e3362a245d8f45cbaf863fc3c01a2cfa20eb4ffe87c")
        ) ]
    in
    List.iter
      (fun (p, expected_bytes) ->
        let computed_bytes = G2.to_compressed_bytes p in
        assert (Bytes.equal expected_bytes computed_bytes))
      vectors

  let get_tests () =
    let open Alcotest in
    let (name, common_tests) = get_tests () in
    ( name,
      test_case "vectors" `Quick test_vectors
      :: test_case "Regression tests" `Quick regression_tests
      :: common_tests )
end

let () =
  let open Alcotest in
  run
    "G2"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests ();
      UncompressedRepresentation.get_tests ();
      CompressedRepresentation.get_tests ();
      Constructors.get_tests () ]
