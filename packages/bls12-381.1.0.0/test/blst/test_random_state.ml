let test_fr () =
  let state = Random.State.make [| 42 |] in
  let x = Bls12_381.Fr.random ~state () in
  let expected_value =
    Bls12_381.Fr.of_string
      "14994049377928826363495223704381650737411020059923663878974688106580230994440"
  in
  if not @@ Bls12_381.Fr.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      (Bls12_381.Fr.to_string expected_value)
      (Bls12_381.Fr.to_string x)

let test_fq12 () =
  let state = Random.State.make [| 42 |] in
  let x = Bls12_381.Fq12.random ~state () in
  let expected_value =
    Bls12_381.Fq12.of_bytes_exn
      (Hex.to_bytes
         (`Hex
           "25f768c109cbed9bdca2f5246b4048e629577e67f0b7b60017b93a676df4525e26571c8bf615a2db454e607d791a900d44547785055f24d6de2fb554b3afb75fe403ce99ca64e9446469f610ef9912cb15a97da27933143fd520e489df4225071672b08c7a9bff225de298772bb2b2b48ccffedb8d95fe7ef8c64c467931d84af028a9e583874c4d3f765d5a83d944059016a069f336b5a029ff3a93ccd3170fd64ff5c103528ffdc03315216aa779017d921fd1072c2576f9b4d5e97f8c9c0db0ab0cdacf46de2fc0b87c5f7309dd69e6b44d4e41c16533ab3114aba08517e7fd25f04a6e5908f9a33ba1da82327008e1d3ae9069f091b0f127daefdae4bb3b25822c0644b265892f9989196f5ae8f1195cbbd7e847af7547f43923afd7c1073808aff305be9fab1ec9b709bdcfd109b0b2354d39ab5747493405c1e97d27ab9185369e1371d7df4dc990c294322c0fb2fc8fce727ca6db9cb5e4a4ca9b250c6b2b2b494e9c5bac9bec4ae79151560afe53eede1c657f5a459434d59e6cbf00f8645d0d9ae1c2165ef3433c6c0362725c64a495d167df503e33e05147646fef45ffd762cb77370669cc0259524f280f3755a09cc1104ea8c3c958186f6f7748608bef4edc700517b6bd310be3cf0229b9d8f93021b099504627b1b9aaa092158be839cbdda3be9fa3d27ba8773106d98cca2018841fe2f11ec0beb53630db7f5552cf0df4b65d272d5de25164a4301898d723df680776dfdf795fa3c0ec7182292a8b77ecc243fabf8cf49378aaa1e2cd6865311ab888a3c10a71d5a91d9109"))
  in
  if not @@ Bls12_381.Fq12.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.Fq12.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.Fq12.to_bytes x)))

let test_g1 () =
  let state = Random.State.make [| 42 |] in
  let x = Bls12_381.G1.random ~state () in
  let expected_value =
    Bls12_381.G1.of_bytes_exn
      (Hex.to_bytes
         (`Hex
           "00e9a2b6bb8b06be174a7d3cbe03e8f4bf2371149124ebb2d11dba1364c588705871df293ef1be2be7f93638b004c3a806993a04535895a92a05e37dedcc0819a7362475eab3befd786569aa9f915a4179f1d60eb0437665dc13dcb2084bc3ef"))
  in
  if not @@ Bls12_381.G1.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.G1.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.G1.to_bytes x)))

let test_g2 () =
  let state = Random.State.make [| 42 |] in
  let x = Bls12_381.G2.random ~state () in
  let expected_value =
    Bls12_381.G2.of_bytes_exn
      (Hex.to_bytes
         (`Hex
           "18f91ca488ba88919f6fc26b9275535a35e9946df913caa0a6ad89334a0e40b99b3898e0658693c2b6b1d764c3d556e417dbba86d110240b503ba58eb56d462a88f6d6c9b49735c76c641fc616c18997928db9cdc6b95e543c944e684ae73f0d08a0e3fd91e9155c705f293c4efc1073b864f26b8b799f140e4389a6d9ae1f82a9d89613fa20b34fd555e2f323a09f7b0b4a56ecd554ea0f84fc30d1e93f26b956d9e78d04583a5bd044f71772bc5a39e187e9c2e9527b2821b1f51435edd77a"))
  in
  if not @@ Bls12_381.G2.(eq x expected_value) then
    Alcotest.failf
      "Expected value is %s but the generated random value is %s"
      Hex.(show (of_bytes (Bls12_381.G2.to_bytes expected_value)))
      Hex.(show (of_bytes (Bls12_381.G2.to_bytes x)))

let () =
  let open Alcotest in
  run
    "Random state"
    [ ( "Random state",
        [ test_case "Fr" `Quick test_fr;
          test_case "Fq12" `Quick test_fq12;
          test_case "G1" `Quick test_g1;
          test_case "G2" `Quick test_g2 ] ) ]
