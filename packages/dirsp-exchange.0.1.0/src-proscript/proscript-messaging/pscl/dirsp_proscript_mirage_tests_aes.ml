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
include Dirsp_proscript_mirage_testvectors_aes_gcm

(* ----------------------------------------------- *)
(*                    AES Tests                    *)
(* ----------------------------------------------- *)

type ciphertext_tag = Ciphertext of (M.t * M.t)

(**
  @param truthy set to false when you want to make a notequals operator.
  *)
let hexbuffer_ciphertext_tag_equals ~truthy =
  Alcotest.testable
    (fun fmt (ttt : ciphertext_tag) ->
      match ttt with
      | Ciphertext (txt, tag) ->
          Format.fprintf fmt "\nciphertext=" ;
          M.hexdump_pp fmt txt ;
          Format.fprintf fmt "\ntag=" ;
          M.hexdump_pp fmt tag )
    (fun ttt1 ttt2 ->
      match (ttt1, ttt2) with
      | Ciphertext (txt1, tag1), Ciphertext (txt2, tag2) ->
          let truth = M.equal txt1 txt2 && M.equal tag1 tag2 in
          truthy = truth )


(** [create_test_xAESGCMEncrypt ~k ~iv ~m ~aad ~ct ~tag ~expectfail ~expecterror] creates a function
    that will do an Alcotest check
    to see whether the AES-GCM encryption of message [m] with key [k] and initialization vector [iv]
    and additional authenticated data [aad] produces the ciphertext [ct]
    and authentication tag [tag] unless [expectfail] is enabled to check for a wrong [ct] / [tag] or unless
    [expecterror] is enabled to capture an anticipated exception *)
let create_test_xAESGCMEncrypt
    ~k ~iv ~m ~aad ~ct ~tag ~expectfail ~expecterror _ =
  let k = buffer_of_hex k in
  let iv = buffer_of_hex iv in
  let msg = buffer_of_hex m in
  let aad = buffer_of_hex aad in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure
         "xAESGCMEncrypt Mirage_crypto: GCM: invalid nonce of length 0" )
      (fun () -> M.Crypto.xAESGCMEncrypt k iv msg aad |> ignore)
  else if expectfail
  then
    Alcotest.(check (hexbuffer_ciphertext_tag_equals ~truthy:false))
      "not same cipher/plaintype text tag"
      (Ciphertext (buffer_of_hex ct, buffer_of_hex tag))
      (let result = M.Crypto.xAESGCMEncrypt k iv msg aad in
       Ciphertext (result.ciphertext, result.tag) )
  else
    Alcotest.(check (hexbuffer_ciphertext_tag_equals ~truthy:true))
      "same cipher/plaintype text tag"
      (Ciphertext (buffer_of_hex ct, buffer_of_hex tag))
      (let result = M.Crypto.xAESGCMEncrypt k iv msg aad in
       Ciphertext (result.ciphertext, result.tag) )


(** [create_test_xAESGCMDecrypt ~ct ~tag ~k ~iv ~aad ~pt ~expectfail ~expecterror] creates a function
    that will do an Alcotest check
    to see whether the AES-GCM decryption of ciphertext [ct] and authentication [tag]
    with key [k] and initialization vector [iv] and additional authenticated data [aad]
    produces the plaintext [pt] unless [expectfail] is enabled to check for a wrong [ct] / [tag] or unless
    [expecterror] is enabled to capture an anticipated exception *)
let create_test_xAESGCMDecrypt
    ~ct ~tag ~k ~iv ~aad ~pt ~expectfail ~expecterror _ =
  let (encrypted : M.Crypto.aes_encrypted) =
    { ciphertext = buffer_of_hex ct; tag = buffer_of_hex tag }
  in
  let k = buffer_of_hex k in
  let iv = buffer_of_hex iv in
  let aad = buffer_of_hex aad in
  let decrypt_and_raise_if_invalid () =
    let result = M.Crypto.xAESGCMDecrypt k iv encrypted aad in
    if result.valid
    then result
    else Alcotest.(fail "invalid result from xAESGCMDecrypt")
  in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure
         "xAESGCMDecrypt Mirage_crypto: GCM: invalid nonce of length 0" )
      (fun () -> decrypt_and_raise_if_invalid () |> ignore)
  else if expectfail
  then
    Alcotest.(check bool)
      "invalid decryption result"
      false
      (let result = M.Crypto.xAESGCMDecrypt k iv encrypted aad in
       result.valid )
  else
    Alcotest.(check hexbuffer_equals)
      "same plaintype text"
      (buffer_of_hex pt)
      (let result = decrypt_and_raise_if_invalid () in
       if result.valid
       then ()
       else Alcotest.(fail "invalid result from xAESGCMDecrypt") |> ignore ;
       result.plaintext )


let () =
  let open Alcotest in
  let wycheproof_tests =
    polyfill_List_concat_map
      (fun testGroup -> testGroup.tests)
      wycheproof_test_vectors.testGroups
  in
  run
    "AES"
    [ ( "wycheproof_xAESGCMEncrypt"
      , List.map
          (fun wycheproof_test ->
            test_case
              ( "tc"
              ^ string_of_int wycheproof_test.tcId
              ^ ": "
              ^ wycheproof_test.comment )
              `Quick
              (create_test_xAESGCMEncrypt
                 ~k:wycheproof_test.key
                 ~iv:wycheproof_test.iv
                 ~m:wycheproof_test.msg
                 ~aad:wycheproof_test.aad
                 ~ct:wycheproof_test.ct
                 ~tag:wycheproof_test.tag
                 ~expectfail:
                   ("invalid" = wycheproof_test.result)
                   (* Mirage gives an error, as expected, when you don't provide a nonce. *)
                 ~expecterror:
                   ( "invalid" = wycheproof_test.result
                   && List.mem "ZeroLengthIv" wycheproof_test.flags ) ) )
          wycheproof_tests )
    ; ( "wycheproof_xAESGCMDecrypt"
      , List.map
          (fun wycheproof_test ->
            test_case
              ( "tc"
              ^ string_of_int wycheproof_test.tcId
              ^ ": "
              ^ wycheproof_test.comment )
              `Quick
              (create_test_xAESGCMDecrypt
                 ~k:wycheproof_test.key
                 ~iv:wycheproof_test.iv
                 ~ct:wycheproof_test.ct
                 ~tag:wycheproof_test.tag
                 ~pt:wycheproof_test.msg
                 ~aad:wycheproof_test.aad
                 ~expectfail:
                   ("invalid" = wycheproof_test.result)
                   (* Mirage gives an error, as expected, when you don't provide a nonce. *)
                 ~expecterror:
                   ( "invalid" = wycheproof_test.result
                   && List.mem "ZeroLengthIv" wycheproof_test.flags ) ) )
          wycheproof_tests )
    ]
