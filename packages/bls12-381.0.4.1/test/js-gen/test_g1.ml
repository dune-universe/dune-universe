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
  let module StubsG1Uncompressed =
    Bls12_381_js_gen.G1.MakeUncompressedStubs (Stubs_node) in
  let module StubsG1Compressed =
    Bls12_381_js_gen.G1.MakeCompressedStubs (Stubs_node) in
  let module G1Uncompressed =
    Bls12_381_gen.G1.MakeUncompressed (Fr) (StubsG1Uncompressed)
  in
  let module G1Compressed =
    Bls12_381_gen.G1.MakeCompressed (Fr) (StubsG1Compressed)
  in
  let module ValueGeneration = Test_ec_make.MakeValueGeneration (G1Uncompressed) in
  let module IsZero = Test_ec_make.MakeIsZero (G1Uncompressed) in
  let module Equality = Test_ec_make.MakeEquality (G1Uncompressed) in
  let module ECProperties = Test_ec_make.MakeECProperties (G1Uncompressed) in
  let module ValueGenerationCompressed =
    Test_ec_make.MakeValueGeneration (G1Compressed) in
  let module IsZeroCompressed = Test_ec_make.MakeIsZero (G1Compressed) in
  let module EqualityCompressed = Test_ec_make.MakeEquality (G1Compressed) in
  let module ECPropertiesCompressed =
    Test_ec_make.MakeECProperties (G1Compressed) in
  let module Constructors = struct
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
      let g = G1Uncompressed.of_z_opt ~x ~y in
      match g with
      | Some g -> assert (G1Uncompressed.eq G1Uncompressed.one g)
      | None -> assert false

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
      let g = G1Uncompressed.of_z_opt ~x ~y in
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
      let g = G1Uncompressed.of_z_opt ~x ~y in
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
      let g = G1Uncompressed.of_z_opt ~x ~y in
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
      let p1 = G1Uncompressed.of_z_opt ~x:x_1 ~y in
      let x_2 =
        Z.of_string
          "3821408151224848222394078037104966877485040835569514006839342061575586899845797797516352881516922679872117658572470"
      in
      let p2 = G1Uncompressed.of_z_opt ~x:x_2 ~y in
      let x_res =
        Z.of_string
          "52901198670373960614757979459866672334163627229195745167587898707663026648445040826329033206551534205133090753192"
      in
      let y_res =
        Z.of_string
          "1711275103908443722918766889652776216989264073722543507596490456144926139887096946237734327757134898380852225872709"
      in
      let res = G1Uncompressed.of_z_opt ~x:x_res ~y:y_res in
      match (res, p1, p2) with
      | (Some res, Some p1, Some p2) ->
          assert (G1Uncompressed.eq res (G1Uncompressed.add p1 p2))
      | _ -> assert false

    let test_vectors_zero_and_2_not_on_curve () =
      let x = Z.of_string "0" in
      let y = Z.of_string "2" in
      match G1Uncompressed.of_z_opt ~x ~y with
      | Some _ -> assert false
      | None -> assert true

    let test_vectors_zero_and_minus_2_not_on_curve () =
      let x = Z.of_string "0" in
      let y = Z.neg @@ Z.of_string "2" in
      match G1Uncompressed.of_z_opt ~x ~y with
      | Some _ -> assert false
      | None -> assert true

    let test_vectors_random_points_not_on_curve () =
      let x = Z.of_string "90809235435" in
      let y = Z.neg @@ Z.of_string "8090843059809345" in
      match G1Uncompressed.of_z_opt ~x ~y with
      | Some _ -> assert false
      | None -> assert true

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
  end in
  let open Alcotest in
  run
    "G1 Uncompressed"
    [ IsZero.get_tests ();
      ValueGeneration.get_tests ();
      Equality.get_tests ();
      ECProperties.get_tests ();
      Constructors.get_tests () ] ;
  run
    "G1 Compressed"
    [ IsZeroCompressed.get_tests ();
      ValueGenerationCompressed.get_tests ();
      EqualityCompressed.get_tests ();
      ECPropertiesCompressed.get_tests () ]
