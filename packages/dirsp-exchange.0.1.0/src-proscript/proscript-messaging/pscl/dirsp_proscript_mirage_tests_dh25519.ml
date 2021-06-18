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
include Dirsp_proscript_mirage_testvectors_x25519

(* --------------------------------------------------- *)
(*                    DH25519 Tests                    *)
(* --------------------------------------------------- *)

(* We are testing our own wrappers around Mirage, but obviously Mirage
   is getting tested as well. *)

(** [wycheproof_x25519 ~tpublic ~tprivate ~tshared ~expecterror] does an Alcotest check
  to see whether the x25519 key exchange of [tprivate] with [tpublic] produces [tshared]
  unless [expecterror] is enabled to capture an anticipated exception *)
let wycheproof_x25519 ~tpublic ~tprivate ~tshared ~expecterror =
  let scalar = buffer_of_hex tprivate in
  let base = buffer_of_hex tpublic in
  let expected_shared = buffer_of_hex tshared in
  if expecterror
  then
    Alcotest.(check_raises)
      "exception raised"
      (Dirsp_proscript.Crypto_failure "DH25519 key_exchange Low_order")
      (fun () -> M.Crypto.xDH25519 scalar base |> ignore)
  else
    let actual_shared = M.Crypto.xDH25519 scalar base in
    Alcotest.(check hexbuffer_equals)
      "same hex representation of bytes"
      expected_shared
      actual_shared


let () =
  let open Alcotest in
  let wycheproof_tests =
    polyfill_List_concat_map
      (fun testGroup -> testGroup.tests)
      wycheproof_test_vectors.testGroups
  in
  run
    "DH25519"
    [ ( "wycheproof_x25519"
      , List.map
          (fun wycheproof_test ->
            test_case
              ( "tc"
              ^ string_of_int wycheproof_test.tcId
              ^ ": "
              ^ wycheproof_test.comment )
              `Quick
              (fun _ ->
                (*
                NOTE 1:
                When the LowOrderPublic flag is on, Mirage (wisely IMHO)
                does not accept low order public keys! Mirage will produce
                an error, which our code raises as an exception.
                *)
                wycheproof_x25519
                  ~tpublic:wycheproof_test.public
                  ~tprivate:wycheproof_test.xprivate
                  ~tshared:wycheproof_test.shared
                  ~expecterror:(List.mem "LowOrderPublic" wycheproof_test.flags)
                ) )
          wycheproof_tests )
    ]
