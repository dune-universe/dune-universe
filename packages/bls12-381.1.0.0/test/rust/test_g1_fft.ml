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

module G1 = Bls12_381.G1

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
          let bytes_buf = Bytes.create G1.size_in_bytes in
          Stdlib.really_input ic bytes_buf 0 G1.size_in_bytes ;
          G1.of_bytes_exn bytes_buf)
    in
    close_in ic ;
    group_elements

  let test_fft () =
    let power = 2 in
    let m = power2 power in
    let omega_domain = generate_domain 2 m false in
    let g1_elements = parse_group_elements_from_file m "test_vector_g1_2" in
    let result =
      G1.fft ~domain:omega_domain ~points:(Array.of_list g1_elements)
    in
    let expected_result =
      parse_group_elements_from_file m "fft_test_vector_g1_2"
    in
    let result = Array.to_list result in
    let l = List.combine result expected_result in
    List.iter (fun (p1, p2) -> assert (G1.eq p1 p2)) l

  let test_ifft () =
    let power = 2 in
    let m = power2 power in
    let omega_domain = generate_domain power m true in
    let g1_elements = parse_group_elements_from_file m "test_vector_g1_2" in
    let result =
      G1.ifft ~domain:omega_domain ~points:(Array.of_list g1_elements)
    in
    let expected_result =
      parse_group_elements_from_file m "ifft_test_vector_g1_2"
    in
    let result = Array.to_list result in
    let l = List.combine result expected_result in
    List.iter (fun (p1, p2) -> assert (G1.eq p1 p2)) l

  let test_fft_with_greater_domain () =
    (* Vectors generated with the following program:
       ```
       let eval_g1 p x =
         (* evaluation of polynomial p at point x *)
         let h_list = List.rev (Array.to_list p) in
         let aux acc a = G1.(add (mul acc x) a) in
         List.fold_left aux G1.zero h_list
       in
       let g1_to_string x = Hex.show (Hex.of_bytes (G1.to_bytes x)) in
       Random.self_init () ;
       let n = 16 in
       let root =
         Bls12_381.Fr.of_string
           "16624801632831727463500847948913128838752380757508923660793891075002624508302"
       in
       let domain = Array.init n (fun i -> Bls12_381.Fr.pow root (Z.of_int i)) in
       let pts = Array.init (1 + Random.int (n - 1)) (fun _ -> G1.random ()) in
       let result_fft = Array.map (eval_g1 pts) domain in
       Printf.printf
         "Random generated points: [|\n%s\n|]\n"
         (String.concat
            "; "
            Array.(
              to_list (map (fun s -> Printf.sprintf "\"%s\"" (g1_to_string s)) pts))) ;
       Printf.printf
         "Results FFT: [|\n%s\n|]\n"
         (String.concat
            "; "
            Array.(
              to_list
                (map
                   (fun s -> Printf.sprintf "\"%s\"" (g1_to_string s))
                   result_fft)))
       ```
    *)
    let vectors_for_fft_with_greater_domain =
      [ ( [| "11e24ecf7db8b87d8225933d371fd190bfb75532750b0048ff54ebf3da11ed5cfcc2f07441578a64222bf4067817c14a19483158637fcb3dc48a45929d98ed09a4b1cb6e8a27d713449b83aeddb39188f633aea5a099ceead94e563b5ca4cad0";
             "0461ea7440144da988e089841b56ecc6ce89f6a4f1c922f3b0819b71235af74c5b7461c9b0c44621ebba09962be9359502f2ee5c571ee17df5d84e6b549d289fcaec674cdfb53c1619e0a5c2baeb384f2e42c92f211d8a3d27bab0ed79049a86";
             "0f7520186af69f697b690e05beff8104409e7f4669867a711dc59289bc3599f288673f363edd619d7df0780366089c9f08d436096f99cdfba4cee6c9674847e5c6898ef584c51eda818a945416d94cb0565c3e82c76ddf72d0c68d5732f813e7";
             "0b671dadcc18bae5c585894ea6fab93739d991ce180f8f8247b3e6f5007ffed6fbbbceefffce38d5e7195ba8368a52ca09ebf94608ae74dc157a017275bebf5e9be85bef68789b9e3d9c8f433576f4b03f0cfad9f488b9c5f01bd8768933c797";
             "0e74caff4ef6ca037fc1d864db91a6c4e7bae25bb0b03e56ab91b18eabed9703de26b388e36715e270f9de154ff55bad0aad7d05a3a30e82ab2048e6c1400403cbd6db351ed3dd01a96b9341c9af66a75cdb6b5eaba5783a3ebbfc26acdb974b";
             "0d82595066ef27dc4e0191b99b5638ffc1564971b494a2a57a3bcc5d3f6ece59cb53b76a3da5711ab14ae51ad3296f660724e7e2d55a85df80064f323a45cacd5fff10bc8813542df73c7e9b115462b4602ea5a3cc438620fc1f2667f6d231c7";
             "106fb9c62c2cc3574ed292e53325c6d8b9a1175f2c104e9ae0cf9cd944a2c78c18f0419931938b6cbb3dfe623589cb300ee1f2a9f922c9f0b7ab71705f217b56824cb7067e86b506a24908f84da585b852665698ee9bc5fee1896b3c84030d57";
             "15f04ccef3a45638ffe17311bb621fb05aa449d761b23e3061b2919179726bf6924097d3a67cf4b60831f7dcb004b9180923d4e0d86f3ddc15ce46a1f05484c630241aee51ffc293daff2a654c1f5cd2ba93beb3c80719772529eb5ba4b8cc5d";
             "0c9d0717bad1acb84b9d52e1fc3ad9be56a13818526ac1d76d2bd220f2c9888a12083f69826cbf6563fd102497e9f43a0e46f5006374d8e69d401cfb144d5b67b6e709163505215d961e00fb58584e80cda42beaff5e853cb5487c0decc53f44";
             "0e0b2cc89162c99ca1444eca525958ef167a4da63f960d967083bb7982e3729aeba7c5805563ad6e3505f1a3351021d2029453013e2830e4b40ac10447c0a5f1fa412624a79f9652a2e4dd07492a688e79431ecec77cbb57b0bf9265d1cad245";
             "127b09eac3ee658a141822fc8c6f9080dfc1e9bb30157dfd7a32de38c1b7144f9054ae692c00d9af7e01ec896c8d19d7032a43c4276e29d497e85ee2c2c28298cf9d91188444f558b7cad2044a5c882d2dc58dfe55bce87a94f2ba17ce780761";
             "07e0fa94bff559ee91e790bdb52ecfa6b6be7a1185d67f1aa541b0975099571ef42996da7f8c9690f9e4daf4af5a588f1589a33c562c9ea25e7e20fcf56591a73db60703c706b000dfaf1a42bbdb389f78e64c4c8403c607eb4fbba92ad597d3";
             "0d4e499ca03f1b6be5df7234be4f4053ee8e3e01b415bc83b16e77d2e2cd2259402923c114c9fca461c9d574b31ad6b306757e85e8cbc0c3892dfa4fecb3ba370055d0786568c359f5301f2a8560c4d244e03948db50fc612795e67e52c001d5"
          |],
          [| "0f76356edac534b7077f3e3d59ad24c6f42ad40820c52c8691ab9e343f76114147165c415d1c50d8faccd3bf008b77cf07b566895616eedf5f8a4406126651e1b531cfe5827a4cabf838af451e11d5ea9326da646ec00c2c94df5ace4fa0de37";
             "13ba51331d200a7b2b30f46c1a0b0d24b107b7129438d5f3ca5cb241fd952ce60ace04734ad9e248bc046167171e9ece0774e90da31c042d420a7a449f185431b72704290efff501861b4f2b13e90b509eb97b4c82bef19722ac8987225aa68f";
             "175b68e7c0b234a4f4cc76b340875069beecc062654047107a131b1f3d7b22549148df52a18ac9ab1fab2a3ba68577fe19f2715b6e303d4cd3b87e27f11a0e03bb1bbd9f1796444847169e20aeecfd3db2403258ada390e2575e7b91092d69e3";
             "0595c70d05ea31b422fd48bcf978cdf914d4727bc9ac177e03938d9a78059c97e050a172a9d2ac4a7b46895e0c77053000df486db0fc1cf6be16fa81d26c2655796116c268599c384a5d8fad99a3ae36e0b111d110c91e4230b480113b19e63f";
             "0954ebd15bb021394bc95737d11b116452fb268b2016f3efbbd4173dfa1e070aa69562ade3d3b048fed48561c6c7d65a0387d6ddf01c207fcee5a166d24eb9ef043958b41369e2545b22819623ed37634092a1bd935373f89a0dce1b723694ac";
             "18dcace12debcd1b80f99b78cd2b61755d85d67ed7b7c9d815ec8b0e626c219d47305fb2b99547d7e44f5d0b8276c0ba05ef1c477e8a2a75ecf61a84f940254df7dad0581100ab4ed76fa45a6510a2df92058b6eddce30b1e7deceba1d258115";
             "0aff4b784d8fcab23bdf23bdfad07e380570af8dfd7a243e48db36e3f968103a0117e10f2a300a4657ac2b93e955065612d59fa091ee70d194b5e32580ee273acebb7ccd27669be7e79d97bccf2d99f5becee43cb5abfdcf2148853216f7c3e6";
             "14321524b1fd88e4ee5f3e3c558122082310ce83bee0f827bef7581e4ba62a698f809ffce1b0485fe8466a4e6ad445e61228f8cbd04430e82d1e7797d55d3dbe87e2c5f7affba87c84c02c14e4b8b5beb00d14377ab59f76b4cba98343565201";
             "11090e5ab03679e1f559498c88ed915dbae4b3ecbe7c16c81affac9178baa1edf909a6ac9bbd5fa0194f67c80e54adef0b5c445f6869d36e1819467019bc60ad78c7cbbe1943febef673da3f5be5677b4decabcd7f095ba7667b6c0a4b227c33";
             "114e69707065f1afd7772a472fa4ec4b03126a2e39c17d1329cc3d20fc97975161eec444936bfe396c78d31e2905cf6c18363241b07cbef2c948b06fd9e3daf160d33b8f23af7fb636b75f078e9794ff8a0d397821a5f0e9c48f1f0c0f789b57";
             "19699bbd25229e1dc8babd64962504d7c97995dc14340e5e01267511849c9f6771629511367177f7e8439477bf5ce76513461c0814310d1596ca0940fbe476397fbe5d9fe4c8701b3e3750f599827c7f24e3edd206ed9628e479ecf25455d304";
             "0bd469e8035911d6720b562b0d296b138167af6a3ea82edb02137f13942a4e3bb125844fbe954c7e9bf086abc32ae88e18e2b852cce8a7d7c28cf23b9b1bb0ef2dd9ebe034fdff7d96ab5ddc61f5edaca2432bbc96ac38e39cd6bbf1b7dc720a";
             "1984fc4482951c1dc5dfe5f3dabb6d0b02466db7260de07e9ed3b3a2267781ed3638055a4392b8ddfc3925eac7af08c7133fcd5346f8834420618a600ed95a20d635a5c97f52f4288ea6d859a0129efc90e269638cdfe1a813ae9cf2bfab3b0c";
             "08a8693dcda40c3a4f34e68b070976d2772dec5803b69c79bbfae23cd015252689644284b9dc3ad4685ff4fda8b8157814b530fef3380df6014facc5b613312da03d76edb84e4680651a3ec66e4af00c20ea3e4e7b8d3d4c36b3b5fa69107c07";
             "085f7bc7aa3025339bfdd5a61f897a8e17d044507262a810fadd4f27413cf45abf6120e2e06e09f2532699b4233d86c71359cbf0fbf41577b380cbd3bb72b9bcd9f1eb320422ea0d95ebb135ce836db1a751e76006ba553cf17eb9875120d9d3";
             "0e49ca1e9c33b27ad5c544c7c1902b8eafc9a3d1bca2f5742d938f13c001e05c752a85dc39559b8734e16eedf7c0716f12bf6cfabbdb91592c5bb2d790573122a60bd0ee0d96b7a3a262b7d8a916cfd13047d3bc73c39e004fcc46678d57e7d2"
          |],
          "16624801632831727463500847948913128838752380757508923660793891075002624508302",
          16 );
        ( [| "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e"
          |],
          [| "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e";
             "0d2a681e0762ea393c40c87ead9aafbcf075e9e6eaae5795ae5f51ef91643389fa9365c9b7e6aa192ae5dc18241e282f18f1eccd871627f6c37da302e5aeef08c9ada3f5a1ab7dce13245daa9bd2539f193e4ec480016fcbc30a9b346049384e"
          |],
          "16624801632831727463500847948913128838752380757508923660793891075002624508302",
          16 ) ]
    in

    let of_string x = G1.of_bytes_exn (Hex.to_bytes (`Hex x)) in
    List.iter
      (fun (points, expected_fft_results, root, n) ->
        let root = Bls12_381.Fr.of_string root in
        let points = Array.map of_string points in
        let expected_fft_results = Array.map of_string expected_fft_results in
        let domain =
          Array.init n (fun i -> Bls12_381.Fr.pow root (Z.of_int i))
        in
        let fft_results = G1.fft ~domain ~points in
        Array.iter2
          (fun p1 p2 ->
            let g1_to_string x = Hex.show (Hex.of_bytes (G1.to_bytes x)) in
            if not (G1.eq p1 p2) then
              Alcotest.failf
                "Expected FFT result %s\nbut the computed value is %s\n"
                (g1_to_string p1)
                (g1_to_string p2))
          expected_fft_results
          fft_results)
      vectors_for_fft_with_greater_domain

  let get_tests () =
    let open Alcotest in
    ( "(i)FFT of G1 Uncompressed",
      [test_case "fft" `Quick test_fft; test_case "ifft" `Quick test_ifft] )
end

let () =
  let open Alcotest in
  run "G1 FFT Uncompressed" [FFT.get_tests ()]
