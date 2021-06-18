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
include Dirsp_proscript_mirage_testvectors_hmac_sha256

(* ----------------------------------------------- *)
(*                   Hash Tests                    *)
(* ----------------------------------------------- *)

(** [create_test_hmacsha256 ~k ~m ~tag ~expectfail ~expecterror] creates a function
    that will do an Alcotest check
    to see whether the HMAC-SHA-256 of message [m] under key [k] produces the authentication
    code [tag] unless [expectfail] is enabled to check for a wrong [tag] or unless
    [expecterror] is enabled to capture an anticipated exception *)
let create_test_hmacsha256 ~k ~m ~tag ~expectfail ~expecterror _ =
  let key = buffer_of_hex k in
  let msg = buffer_of_hex m in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure "DH25519 key_exchange Low_order")
      (fun () -> M.Crypto.xHMACSHA256 key msg |> ignore)
  else if expectfail
  then
    Alcotest.(check hexbuffer_notequals)
      "not same hex digits"
      (buffer_of_hex tag)
      (M.Crypto.xHMACSHA256 key msg)
  else
    Alcotest.(check hexbuffer_equals)
      "same hex digits"
      (buffer_of_hex tag)
      (M.Crypto.xHMACSHA256 key msg)


let create_test_sha256 ~m ~hash _ =
  Alcotest.(check hexbuffer_equals)
    "xSHA256 is hexadecimal encoded bytes"
    (M.of_string (strip_internal_whitespace hash))
    (M.Crypto.xSHA256 (buffer_of_hex m))


let create_test_sha512 ~m ~hash _ =
  Alcotest.(check hexbuffer_equals)
    "xSHA512 is hexadecimal encoded bytes"
    (M.of_string (strip_internal_whitespace hash))
    (M.Crypto.xSHA512 (buffer_of_hex m))


let () =
  let open Alcotest in
  let one_million_letter_a_in_hex =
    String.init 2_000_000 (fun i ->
        (* 1,000,000 "61", which is 'a' in hexadecimal *)
        if i mod 2 = 0 then '6' else '1' )
  in
  let wycheproof_tests =
    polyfill_List_concat_map
      (* For some reason wycheproof include tests for shorter tags (tagSize=128) rather than the
         full 256-bit digest tag of HMAC-SHA-256. We skip over those tests because ProScript has no
         API that uses short digests *)
        (fun testGroup ->
        if testGroup.tagSize = 256 then testGroup.tests else [] )
      wycheproof_test_vectors.testGroups
  in
  run
    "Hash"
    [ ( "wycheproof_hmacsha256"
      , List.map
          (fun wycheproof_test ->
            test_case
              ( "tc"
              ^ string_of_int wycheproof_test.tcId
              ^ ": "
              ^ wycheproof_test.comment )
              `Quick
              (create_test_hmacsha256
                 ~k:wycheproof_test.key
                 ~tag:wycheproof_test.tag
                 ~m:wycheproof_test.msg
                 ~expectfail:("invalid" = wycheproof_test.result)
                 ~expecterror:false ) )
          wycheproof_tests )
    ; ( "litzenberger_sha256"
      , (* Tests from https://www.dlitz.net/crypto/shad256-test-vectors/ *)
        [ test_case
            "EMPTY"
            `Quick
            (create_test_sha256
               ~hash:
                 "e3b0c44298fc1c149afbf4c8996fb924\n\
                  27ae41e4649b934ca495991b7852b855"
               ~m:"" )
        ; test_case
            "NIST.1"
            `Quick
            (create_test_sha256
               ~hash:
                 "ba7816bf8f01cfea414140de5dae2223\n\
                  b00361a396177a9cb410ff61f20015ad"
               ~m:"616263" (* 'abc' in hex *) )
        ; test_case
            "NIST.3"
            `Quick
            (create_test_sha256
               ~hash:
                 "cdc76e5c9914fb9281a1c7e284d73e67\n\
                  f1809a48a497200e046d39ccc7112cd0"
               ~m:one_million_letter_a_in_hex )
        ; test_case
            "RC4.16"
            `Quick
            (create_test_sha256
               ~hash:
                 "067c531269735ca7f541fdaca8f0dc76\n\
                  305d3cada140f89372a410fe5eff6e4d"
               ~m:"de188941a3375d3a8a061e67576e926d" )
        ; test_case
            "RC4.55"
            `Quick
            (create_test_sha256
               ~hash:
                 "038051e9c324393bd1ca1978dd0952c2\n\
                  aa3742ca4f1bd5cd4611cea83892d382"
               ~m:
                 "de188941a3375d3a8a061e67576e926d\n\
                  c71a7fa3f0cceb97452b4d3227965f9e\n\
                  a8cc75076d9fb9c5417aa5cb30fc2219\n\
                  8b34982dbb629e" )
        ] )
    ; ( "di_management_sha512"
      , (* https://www.di-mgt.com.au/sha_testvectors.html *)
        [ test_case
            "abc"
            `Quick
            (create_test_sha512
               ~hash:
                 "ddaf35a193617aba cc417349ae204131 12e6fa4e89a97ea2 \
                  0a9eeee64b55d39a 2192992a274fc1a8 36ba3c23a3feebbd \
                  454d4423643ce80e 2a9ac94fa54ca49f"
               ~m:"616263" )
        ; test_case
            "empty string"
            `Quick
            (create_test_sha512
               ~hash:
                 "cf83e1357eefb8bd f1542850d66d8007 d620e4050b5715dc \
                  83f4a921d36ce9ce 47d0d13c5d85f2b0 ff8318d2877eec2f \
                  63b931bd47417a81 a538327af927da3e"
               ~m:"" )
        ; test_case
            "one million 'a'"
            `Quick
            (create_test_sha512
               ~hash:
                 "e718483d0ce76964 4e2e42c7bc15b463 8e1f98b13b204428 \
                  5632a803afa973eb de0ff244877ea60a 4cb0432ce577c31b \
                  eb009c5c2c49aa2e 4eadb217ad8cc09b"
               ~m:one_million_letter_a_in_hex )
        ] )
    ]
