(* Copyright 2021 Diskuv, Inc.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License. *)
open Dirsp_proscript_mirage_test_helpers

(* --------------------------------------------------- *)
(*                   EDH25519 Tests                    *)
(* --------------------------------------------------- *)

(* We are testing our own wrappers around Mirage, but obviously Mirage
   is getting tested as well. *)

(** [test_publicKey ~tpublic ~tprivate ~expecterror] does an Alcotest check
  to see whether the x25519 public key [tpublic] is derived from the private
  key [tprivate] unless [expecterror] is enabled to capture an anticipated exception *)
let test_publicKey ~tpublic ~tprivate ~expecterror =
  let sk = buffer_of_hex tprivate in
  let expected_public = buffer_of_hex tpublic in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure "EDH25519 publicKey Low_order")
      (fun () -> M.Crypto.ED25519.publicKey sk |> ignore)
  else
    let actual_public = M.Crypto.ED25519.publicKey sk in
    Alcotest.(check hexbuffer_equals)
      "same hex representation of bytes"
      expected_public
      actual_public


(** [test_verify ~tpublic ~tmessage ~tsignature ~expecterror] does an Alcotest check
  to see whether the signature [tsignature] of the message [tmessage] was produced by a private
  key corresponding to x25519 public key [tpublic] unless [expecterror] is enabled to capture an
  anticipated exception *)
let test_verify ~tpublic ~tmessage ~tsignature ~expecterror =
  let signature = buffer_of_hex tsignature in
  let msg = buffer_of_hex tmessage in
  let pub = buffer_of_hex tpublic in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure "EDH25519 publicKey Low_order")
      (fun () -> M.Crypto.ED25519.checkValid signature msg pub |> ignore)
  else
    let actual_validity = M.Crypto.ED25519.checkValid signature msg pub in
    Alcotest.(check bool) "valid" true actual_validity


(** [test_signature ~tprivate ~tpublic ~tmessage ~tsignature ~expecterror] does an Alcotest check
  to see whether we match the given signature [tsignature] with the signature calculated from
  the message [tmessage] using a private key [tprivate] and its corresponding public key
  [tpublic] unless [expecterror] is enabled to capture an anticipated exception *)
let test_signature ~tprivate ~tpublic ~tmessage ~tsignature ~expecterror =
  let msg = buffer_of_hex tmessage in
  let sk = buffer_of_hex tprivate in
  let pk = buffer_of_hex tpublic in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure "EDH25519 signature Low_order")
      (fun () -> M.Crypto.ED25519.signature msg sk pk |> ignore)
  else
    let actual_validity = M.Crypto.ED25519.signature msg sk pk in
    Alcotest.(check hexbuffer_equals)
      "valid"
      (buffer_of_hex tsignature)
      actual_validity


(* Test cases from RFC 8032 https://datatracker.ietf.org/doc/html/rfc8032#section-7.1 *)

type case =
  { tprivate : string
  ; tpublic : string
  ; tmessage : string
  ; tsignature : string
  }

let case_1 =
  { tprivate =
      "9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60"
  ; tpublic = "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  ; tmessage = ""
  ; tsignature =
      "e5564300c360ac729086e2cc806e828a\n\
       84877f1eb8e5d974d873e06522490155\n\
       5fb8821590a33bacc61e39701cf9b46b\n\
       d25bf5f0595bbe24655141438e7a100b"
  }


let case_2 =
  { tprivate =
      "4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb"
  ; tpublic = "3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c"
  ; tmessage = "72"
  ; tsignature =
      "92a009a9f0d4cab8720e820b5f642540\n\
       a2b27b5416503f8fb3762223ebdb69da\n\
       085ac1e43e15996e458f3613d0f11d8c\n\
       387b2eaeb4302aeeb00d291612bb0c00"
  }


let case_3 =
  { tprivate =
      "c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7"
  ; tpublic = "fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025"
  ; tmessage = "af82"
  ; tsignature =
      "6291d657deec24024827e69c3abe01a3\n\
       0ce548a284743a445e3680d7db5ac3ac\n\
       18ff9b538d16f290ae67f760984dc659\n\
       4a7c15e9716ed28dc027beceea1ec40a"
  }


let case_1024 =
  { tprivate =
      "f5e5767cf153319517630f226876b86c8160cc583bc013744c6bf255f5cc0ee5"
  ; tpublic = "278117fc144c72340f67d0f2316e8386ceffbf2b2428c9c51fef7c597f1d426e"
  ; tmessage =
      "08b8b2b733424243760fe426a4b54908\n\
       632110a66c2f6591eabd3345e3e4eb98\n\
       fa6e264bf09efe12ee50f8f54e9f77b1\n\
       e355f6c50544e23fb1433ddf73be84d8\n\
       79de7c0046dc4996d9e773f4bc9efe57\n\
       38829adb26c81b37c93a1b270b20329d\n\
       658675fc6ea534e0810a4432826bf58c\n\
       941efb65d57a338bbd2e26640f89ffbc\n\
       1a858efcb8550ee3a5e1998bd177e93a\n\
       7363c344fe6b199ee5d02e82d522c4fe\n\
       ba15452f80288a821a579116ec6dad2b\n\
       3b310da903401aa62100ab5d1a36553e\n\
       06203b33890cc9b832f79ef80560ccb9\n\
       a39ce767967ed628c6ad573cb116dbef\n\
       efd75499da96bd68a8a97b928a8bbc10\n\
       3b6621fcde2beca1231d206be6cd9ec7\n\
       aff6f6c94fcd7204ed3455c68c83f4a4\n\
       1da4af2b74ef5c53f1d8ac70bdcb7ed1\n\
       85ce81bd84359d44254d95629e9855a9\n\
       4a7c1958d1f8ada5d0532ed8a5aa3fb2\n\
       d17ba70eb6248e594e1a2297acbbb39d\n\
       502f1a8c6eb6f1ce22b3de1a1f40cc24\n\
       554119a831a9aad6079cad88425de6bd\n\
       e1a9187ebb6092cf67bf2b13fd65f270\n\
       88d78b7e883c8759d2c4f5c65adb7553\n\
       878ad575f9fad878e80a0c9ba63bcbcc\n\
       2732e69485bbc9c90bfbd62481d9089b\n\
       eccf80cfe2df16a2cf65bd92dd597b07\n\
       07e0917af48bbb75fed413d238f5555a\n\
       7a569d80c3414a8d0859dc65a46128ba\n\
       b27af87a71314f318c782b23ebfe808b\n\
       82b0ce26401d2e22f04d83d1255dc51a\n\
       ddd3b75a2b1ae0784504df543af8969b\n\
       e3ea7082ff7fc9888c144da2af58429e\n\
       c96031dbcad3dad9af0dcbaaaf268cb8\n\
       fcffead94f3c7ca495e056a9b47acdb7\n\
       51fb73e666c6c655ade8297297d07ad1\n\
       ba5e43f1bca32301651339e22904cc8c\n\
       42f58c30c04aafdb038dda0847dd988d\n\
       cda6f3bfd15c4b4c4525004aa06eeff8\n\
       ca61783aacec57fb3d1f92b0fe2fd1a8\n\
       5f6724517b65e614ad6808d6f6ee34df\n\
       f7310fdc82aebfd904b01e1dc54b2927\n\
       094b2db68d6f903b68401adebf5a7e08\n\
       d78ff4ef5d63653a65040cf9bfd4aca7\n\
       984a74d37145986780fc0b16ac451649\n\
       de6188a7dbdf191f64b5fc5e2ab47b57\n\
       f7f7276cd419c17a3ca8e1b939ae49e4\n\
       88acba6b965610b5480109c8b17b80e1\n\
       b7b750dfc7598d5d5011fd2dcc5600a3\n\
       2ef5b52a1ecc820e308aa342721aac09\n\
       43bf6686b64b2579376504ccc493d97e\n\
       6aed3fb0f9cd71a43dd497f01f17c0e2\n\
       cb3797aa2a2f256656168e6c496afc5f\n\
       b93246f6b1116398a346f1a641f3b041\n\
       e989f7914f90cc2c7fff357876e506b5\n\
       0d334ba77c225bc307ba537152f3f161\n\
       0e4eafe595f6d9d90d11faa933a15ef1\n\
       369546868a7f3a45a96768d40fd9d034\n\
       12c091c6315cf4fde7cb68606937380d\n\
       b2eaaa707b4c4185c32eddcdd306705e\n\
       4dc1ffc872eeee475a64dfac86aba41c\n\
       0618983f8741c5ef68d3a101e8a3b8ca\n\
       c60c905c15fc910840b94c00a0b9d0"
  ; tsignature =
      "0aab4c900501b3e24d7cdf4663326a3a\n\
       87df5e4843b2cbdb67cbf6e460fec350\n\
       aa5371b1508f9f4528ecea23c436d94b\n\
       5e8fcd4f681e30a6ac00a9704a188a03"
  }


let case_sha_abc =
  { tprivate =
      "833fe62409237b9d62ec77587520911e9a759cec1d19755b7da901b96dca3d42"
  ; tpublic = "ec172b93ad5e563bf4932c70e1245034c35467ef2efd4d64ebf819683467e2bf"
  ; tmessage =
      "ddaf35a193617abacc417349ae204131\n\
       12e6fa4e89a97ea20a9eeee64b55d39a\n\
       2192992a274fc1a836ba3c23a3feebbd\n\
       454d4423643ce80e2a9ac94fa54ca49f"
  ; tsignature =
      "dc2a4459e7369633a52b1bf277839a00\n\
       201009a3efbf3ecb69bea2186c26b589\n\
       09351fc9ac90b3ecfdfbc7c66431e030\n\
       3dca179c138ac17ad9bef1177331a704"
  }


let create_test_publicKey (c : case) _ =
  test_publicKey ~tprivate:c.tprivate ~tpublic:c.tpublic ~expecterror:false


let create_test_verify (c : case) _ =
  test_verify
    ~tmessage:c.tmessage
    ~tsignature:c.tsignature
    ~tpublic:c.tpublic
    ~expecterror:false


let create_test_signature (c : case) _ =
  test_signature
    ~tprivate:c.tprivate
    ~tmessage:c.tmessage
    ~tsignature:c.tsignature
    ~tpublic:c.tpublic
    ~expecterror:false


let () =
  let open Alcotest in
  run
    "EDH25519"
    [ ( "RFC_8032_publicKey"
      , [ test_case "Sec 7.1.1" `Quick (create_test_publicKey case_1)
        ; test_case "Sec 7.1.2" `Quick (create_test_publicKey case_2)
        ; test_case "Sec 7.1.3" `Quick (create_test_publicKey case_3)
        ; test_case "Sec 7.1.1024" `Quick (create_test_publicKey case_1024)
        ; test_case
            "Sec 7.1.SHA(abc)"
            `Quick
            (create_test_publicKey case_sha_abc)
        ] )
    ; ( "RFC_8032_verify"
      , [ test_case "Sec 7.1.1" `Quick (create_test_verify case_1)
        ; test_case "Sec 7.1.2" `Quick (create_test_verify case_2)
        ; test_case "Sec 7.1.3" `Quick (create_test_verify case_3)
        ; test_case "Sec 7.1.1024" `Quick (create_test_verify case_1024)
        ; test_case "Sec 7.1.SHA(abc)" `Quick (create_test_verify case_sha_abc)
        ] )
    ; ( "RFC_8032_calculate"
      , [ test_case "Sec 7.1.1" `Quick (create_test_signature case_1)
        ; test_case "Sec 7.1.2" `Quick (create_test_signature case_2)
        ; test_case "Sec 7.1.3" `Quick (create_test_signature case_3)
        ; test_case "Sec 7.1.1024" `Quick (create_test_signature case_1024)
        ; test_case
            "Sec 7.1.SHA(abc)"
            `Quick
            (create_test_signature case_sha_abc)
        ] )
    ]
