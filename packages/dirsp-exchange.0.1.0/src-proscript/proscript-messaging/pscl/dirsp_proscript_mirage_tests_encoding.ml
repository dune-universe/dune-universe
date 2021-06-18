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

(* ----------------------------------------------- *)
(*                 Encoding Tests                  *)
(* ----------------------------------------------- *)

let create_test_b2h c h _ =
  Alcotest.(check hexbuffer_equals)
    "same hex digits"
    (M.of_string h)
    (M.Encoding.b2h (M.elem_of_char c))


let test_hexStringTo12ByteArray () =
  Alcotest.(check hexbuffer_equals)
    "same hex digits"
    (M.of_string "\x01\x23\x45\x67\x89\xab\xcd\xef\x00\x11\x22\x33")
    (M.Encoding.hexStringTo12ByteArray
       (M.of_string "0123456789abcdef00112233") )


let test_hexStringTo32ByteArray () =
  Alcotest.(check hexbuffer_equals)
    "same hex digits"
    (M.of_string
       "\x01\x23\x45\x67\x89\xab\xcd\xef\x00\x11\x22\x33\x44\x55\x66\x77\x88\x99\xaa\xbb\xcc\xdd\xee\xff\xfe\xdc\xba\x98\x76\x54\x32\x10" )
    (M.Encoding.hexStringTo32ByteArray
       (M.of_string
          "0123456789abcdef00112233445566778899aabbccddeefffedcba9876543210" ) )


let () =
  let open Alcotest in
  run
    "Encoding"
    [ ( "b2h"
      , [ test_case "00" `Quick (create_test_b2h '\x00' "00")
        ; test_case "01" `Quick (create_test_b2h '\x01' "01")
        ; test_case "02" `Quick (create_test_b2h '\x02' "02")
        ; test_case "03" `Quick (create_test_b2h '\x03' "03")
        ; test_case "04" `Quick (create_test_b2h '\x04' "04")
        ; test_case "05" `Quick (create_test_b2h '\x05' "05")
        ; test_case "06" `Quick (create_test_b2h '\x06' "06")
        ; test_case "07" `Quick (create_test_b2h '\x07' "07")
        ; test_case "08" `Quick (create_test_b2h '\x08' "08")
        ; test_case "09" `Quick (create_test_b2h '\x09' "09")
        ; test_case "0a" `Quick (create_test_b2h '\x0a' "0a")
        ; test_case "0b" `Quick (create_test_b2h '\x0b' "0b")
        ; test_case "0c" `Quick (create_test_b2h '\x0c' "0c")
        ; test_case "0d" `Quick (create_test_b2h '\x0d' "0d")
        ; test_case "0e" `Quick (create_test_b2h '\x0e' "0e")
        ; test_case "0f" `Quick (create_test_b2h '\x0f' "0f")
        ; test_case "10" `Quick (create_test_b2h '\x10' "10")
        ; test_case "20" `Quick (create_test_b2h '\x20' "20")
        ; test_case "30" `Quick (create_test_b2h '\x30' "30")
        ; test_case "40" `Quick (create_test_b2h '\x40' "40")
        ; test_case "50" `Quick (create_test_b2h '\x50' "50")
        ; test_case "60" `Quick (create_test_b2h '\x60' "60")
        ; test_case "70" `Quick (create_test_b2h '\x70' "70")
        ; test_case "80" `Quick (create_test_b2h '\x80' "80")
        ; test_case "90" `Quick (create_test_b2h '\x90' "90")
        ; test_case "a0" `Quick (create_test_b2h '\xa0' "a0")
        ; test_case "b0" `Quick (create_test_b2h '\xb0' "b0")
        ; test_case "c0" `Quick (create_test_b2h '\xc0' "c0")
        ; test_case "d0" `Quick (create_test_b2h '\xd0' "d0")
        ; test_case "e0" `Quick (create_test_b2h '\xe0' "e0")
        ; test_case "f0" `Quick (create_test_b2h '\xf0' "f0")
        ; test_case "ff" `Quick (create_test_b2h '\xff' "ff")
        ] )
    ; ( "hex-strings"
      , [ test_case "hexStringTo12ByteArray" `Quick test_hexStringTo12ByteArray
        ; test_case "hexStringTo32ByteArray" `Quick test_hexStringTo32ByteArray
        ] )
    ]
