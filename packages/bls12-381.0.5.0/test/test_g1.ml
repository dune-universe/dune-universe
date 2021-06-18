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

(** Check the routine generators do not raise any exception *)

module G1 = Bls12_381.G1
module ValueGeneration = Test_ec_make.MakeValueGeneration (G1)
module IsZero = Test_ec_make.MakeIsZero (G1)
module Equality = Test_ec_make.MakeEquality (G1)
module ECProperties = Test_ec_make.MakeECProperties (G1)

module Constructors = struct
  let test_of_z_one () =
    (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/fq.rs#L18 *)
    let x =
      Z.of_string
        "3685416753713387016781088315183077757961620795782546409894578378688607592378376318836054947676345821548104185464507"
    in
    let y =
      Z.of_string
        "1339506544944476473020471379941921221584933875938349620426543736416511423956333506472724655353366534992391756441569"
    in
    let g = G1.of_z_opt ~x ~y in
    match g with Some g -> assert (G1.eq G1.one g) | None -> assert false

  (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/ec.rs#L1196 *)
  (* https://github.com/zcash/librustzcash/blob/0.1.0/pairing/src/bls12_381/ec.rs#L1245 *)
  let test_vectors_1 () =
    let x =
      Z.of_string
        "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
    in
    let y =
      Z.of_string
        "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
    in
    let g = G1.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_2 () =
    let x =
      Z.of_string
        "128100205326445210408953809171070606737678357140298133325128175840781723996595026100005714405541449960643523234125"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let g = G1.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_3 () =
    let x =
      Z.of_string
        "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let g = G1.of_z_opt ~x ~y in
    match g with Some _ -> assert true | None -> assert false

  let test_vectors_add () =
    let x_1 =
      Z.of_string
        "128100205326445210408953809171070606737678357140298133325128175840781723996595026100005714405541449960643523234125"
    in
    let y =
      Z.of_string
        "2291134451313223670499022936083127939567618746216464377735567679979105510603740918204953301371880765657042046687078"
    in
    let p1 = G1.of_z_opt ~x:x_1 ~y in
    let x_2 =
      Z.of_string
        "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
    in
    let p2 = G1.of_z_opt ~x:x_2 ~y in
    let x_res =
      Z.of_string
        "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
    in
    let y_res =
      Z.of_string
        "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
    in
    let res = G1.of_z_opt ~x:x_res ~y:y_res in
    match (res, p1, p2) with
    | (Some res, Some p1, Some p2) -> assert (G1.eq res (G1.add p1 p2))
    | _ -> assert false

  let test_vectors_zero_and_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.of_string "2" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let test_vectors_zero_and_minus_2_not_on_curve () =
    let x = Z.of_string "0" in
    let y = Z.neg @@ Z.of_string "2" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let test_vectors_random_points_not_on_curve () =
    let x = Z.of_string "90809235435" in
    let y = Z.neg @@ Z.of_string "8090843059809345" in
    match G1.of_z_opt ~x ~y with Some _ -> assert false | None -> assert true

  let get_tests () =
    let open Alcotest in
    ( "From Z elements",
      [ test_case "one (generator)" `Quick test_of_z_one;
        test_case "test vectors 1" `Quick test_vectors_1;
        test_case "test vectors 2" `Quick test_vectors_2;
        test_case "test vectors 3" `Quick test_vectors_3;
        test_case
          "test random points not on curve"
          `Quick
          test_vectors_random_points_not_on_curve;
        test_case
          "test vectors zero and 2 not on curve"
          `Quick
          test_vectors_zero_and_2_not_on_curve;
        test_case
          "test vectors zero and minus 2 not on curve"
          `Quick
          test_vectors_zero_and_minus_2_not_on_curve;
        test_case "test vectors add" `Quick test_vectors_add ] )
end

module UncompressedRepresentation = struct
  let test_uncompressed_zero_has_first_byte_at_64 () =
    assert (int_of_char (Bytes.get G1.(to_bytes zero) 0) = 64)

  let test_uncompressed_random_has_first_byte_strictly_lower_than_64 () =
    assert (int_of_char (Bytes.get G1.(to_bytes (random ())) 0) < 64)

  let test_of_bytes_exn_to_bytes_consistent_on_random () =
    let r = G1.random () in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let test_bytes () =
    let test_vectors =
      [ ( G1.one,
          Bytes.of_string
            "\023\241\211\1671\151\215\148&\149c\140O\169\172\015\195h\140O\151t\185\005\161N:?\023\027\172XlU\232?\249z\026\239\251:\240\n\
             \219\"\198\187\b\179\244\129\227\170\160\241\160\1580\237t\029\138\228\252\245\224\149\213\208\n\
             \246\000\219\024\203,\004\179\237\208<\199D\162\136\138\228\012\170#)F\197\231\225"
        );
        ( G1.zero,
          Bytes.of_string
            "@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
        ) ]
    in
    List.iter
      (fun (v, expected_value) ->
        assert (G1.(eq v (G1.of_bytes_exn expected_value))))
      test_vectors

  let test_of_bytes_exn_to_bytes_consistent_on_one () =
    let r = G1.one in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let test_of_bytes_exn_to_bytes_consistent_on_zero () =
    let r = G1.zero in
    assert (G1.(eq (of_bytes_exn (to_bytes r)) r))

  let get_tests () =
    let open Alcotest in
    ( "Representation of G1 Uncompressed",
      [ test_case
          "zero has first byte at 64"
          `Quick
          test_uncompressed_zero_has_first_byte_at_64;
        test_case
          "of bytes and to bytes are consistent on random"
          `Quick
          (Test_ec_make.repeat
             1000
             test_of_bytes_exn_to_bytes_consistent_on_random);
        test_case "bytes encoding" `Quick test_bytes;
        test_case
          "of bytes and to bytes are consistent on one"
          `Quick
          test_of_bytes_exn_to_bytes_consistent_on_one;
        test_case
          "of bytes and to bytes are consistent on zero"
          `Quick
          test_of_bytes_exn_to_bytes_consistent_on_zero;
        test_case
          "random has first byte strictly lower than 64"
          `Quick
          (Test_ec_make.repeat
             1000
             test_uncompressed_random_has_first_byte_strictly_lower_than_64) ]
    )
end

module CompressedRepresentation = struct
  include Test_ec_make.MakeCompressedRepresentation (G1)

  let test_vectors () =
    let vectors =
      [ ( Hex.to_bytes
            (`Hex
              "97f1d3a73197d7942695638c4fa9ac0fc3688c4f9774b905a14e3a3f171bac586c55e83ff97a1aeffb3af00adb22c6bb"),
          G1.to_compressed_bytes G1.one );
        ( Hex.to_bytes
            (`Hex
              "c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
          G1.to_compressed_bytes G1.zero ) ]
    in
    List.iter
      (fun (expected_bytes, computed_bytes) ->
        assert (Bytes.equal expected_bytes computed_bytes))
      vectors

  (* Random points generated for regression tests
     ```ocaml
     let () =
       let x = Bls12_381.G1.random () in
       Printf.printf
         "Compressed: %s\nUncompressed: %s\n"
         Hex.(show (of_bytes (Bls12_381.G1.to_compressed_bytes x)))
         Hex.(show (of_bytes (Bls12_381.G1.to_bytes x)))
     ```
  *)
  let regression_tests () =
    let vectors =
      [ ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "0cb0050dcf2f82c75c431f1fd4eb44acc526dbc70842c173bf6e10e48482d54c334b25256e1c619e095443a139c159c715a8f06580d0c818ab623b8a7e31d961c28b8e1ee33ab2e60089ac887db42790f704043d3cb7a5bad69e3b4cf312c1be"),
          Hex.to_bytes
            (`Hex
              "acb0050dcf2f82c75c431f1fd4eb44acc526dbc70842c173bf6e10e48482d54c334b25256e1c619e095443a139c159c7")
        );
        ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "15066d35ee5ff3d2b7c6f56e59770cb74eeb0cec96e964bc06faa95fe9ad43adfd77eed86c79af4bd70fdafab703daf8008b67f7b37d345969daed600bd1f6032ab209b0793dd8029e587aa18f463fe63e2909868fb719eab6fbd659dfb3910a"),
          Hex.to_bytes
            (`Hex
              "95066d35ee5ff3d2b7c6f56e59770cb74eeb0cec96e964bc06faa95fe9ad43adfd77eed86c79af4bd70fdafab703daf8")
        );
        ( G1.of_bytes_exn
          @@ Hex.to_bytes
               (`Hex
                 "12a57cbec8a77a1d2e78397910bfff2c53e2a42add9219ab7282d37d668e7e55573a0195baad77c8c4daab5cb777306812919ee53ce703dc9564a2c60dd43641b4950feec26f6146d69bd00e4d329c8a2db86e294d549dd6a04f00a085880517"),
          Hex.to_bytes
            (`Hex
              "b2a57cbec8a77a1d2e78397910bfff2c53e2a42add9219ab7282d37d668e7e55573a0195baad77c8c4daab5cb7773068")
        ) ]
    in
    List.iter
      (fun (p, expected_bytes) ->
        let computed_bytes = G1.to_compressed_bytes p in
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
    "G1"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests ();
      UncompressedRepresentation.get_tests ();
      CompressedRepresentation.get_tests ();
      Constructors.get_tests () ]
