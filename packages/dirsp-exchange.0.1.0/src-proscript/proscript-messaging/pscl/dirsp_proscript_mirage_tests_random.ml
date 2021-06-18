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
(*                 Random Number Tests                 *)
(* --------------------------------------------------- *)

(*
  Use the "overlapping-pairs-sparse-occupancy" (OPSO) test from the Diehard tests to make sure
  our random numbers are actually random.

  Naturally we are testing mirage's random number generator as well, but the real intent is to make
  sure our code is not accidentally truncating any of the bits when it generates a random sequence of
  12 or 32 bytes. Recall that random12Bytes and random32Bytes are copying from Cstruct_t into Bytes, so
  if we copied off by one that would be catastrophic.

  https://en.wikipedia.org/wiki/Diehard_tests:
  The tests OPSO, OQSO and DNA: OPSO means overlapping-pairs-sparse-occupancy. The OPSO test considers
  2-letter words from an alphabet of 1024 letters. Each letter is determined by a specified ten bits
  from a 32-bit integer in the sequence to be tested. OPSO generates 2^21 (overlapping) 2-letter words
  (from 2^21 + 1 "keystrokes") and counts the number of missing words – that is 2-letter words which do
  not appear in the entire sequence. That count should be very close to normally distributed with mean
  141909, sigma 290. Thus (missingwrds-141909) ÷ 290 should be a standard normal variable. The OPSO
  test takes 32 bits at a time from the test file and uses a designated set of ten consecutive bits.
  It then restarts the file for the next designated 10 bits, and so on.
  *)

(* Initialize the random number generator *)
let () = Mirage_crypto_rng_lwt.initialize ()

type opso_acc_t = { attempts : int32 }

(* Obviously we'll need 4 overlapping 10-bit ranges to cover a full 32-bit number *)

let ten_bits_1 (num : int32) = Int32.logand num 0b1111111111l

let ten_bits_2 (num : int32) =
  Int32.shift_right_logical (Int32.logand num 0b11111111110000000000l) 10


let ten_bits_3 (num : int32) =
  Int32.shift_right_logical
    (Int32.logand num 0b111111111100000000000000000000l)
    20


let ten_bits_4 (num : int32) =
  Int32.shift_right_logical
    (Int32.logand num 0b11111111110000000000000000000000l)
    22


let test_first_ten_bits () =
  Alcotest.(check int32)
    "same bits"
    (ten_bits_1 0b10_1001001010_0100101001_0010100100l)
                0b00_0000000000_0000000000_0010100100l [@@ocamlformat "disable"]

let test_second_ten_bits () =
  Alcotest.(check int32)
    "same bits"
    (ten_bits_2 0b10_1001001010_0100101001_0010100100l)
                0b00_0000000000_0000000000_0100101001l [@@ocamlformat "disable"]

let test_third_ten_bits () =
  Alcotest.(check int32)
    "same bits"
    (ten_bits_3 0b10_1001001010_0100101001_0010100100l)
                0b00_0000000000_0000000000_1001001010l [@@ocamlformat "disable"]

let test_last_ten_bits () =
  Alcotest.(check int32)
    "same bits"
    (ten_bits_4 0b1010010010_1001001010_0100101001_00l)
                0b00_0000000000_0000000000_1010010010l [@@ocamlformat "disable"]

(** [opso_test random_seq first_letter_selector second_letter_selector] takes an infinite sequence [random_seq] of 4 bytes
    and two 10-bit selectors ([first_letter_selector] and [second_letter_selector]) and runs the OPSO test of Diehard
    test's.

    PLEASE NOTE THAT THIS ISN'T A SENSITIVE TEST! The confidence interval is
    wide (4 sigmas) so false negatives can and will occur.
    In reality, there are _no_ sensitive tests for randomness. You just have to live
    in a real world of uncertainty.

    @return true if and only if the missing words are normally distributed within a 4*sigma (99.994%) confidence range
      of the expected random distribution. Said another way, we expect this test to fail 1 out of 15,787 times if the
      given random sequence is truly random (and much more often if it is not random)
*)
let opso_test
    (name : string)
    (random_seq : (char * char * char * char) Iter.t)
    (first_letter_selector : int32 -> int32)
    (second_letter_selector : int32 -> int32) =
  let max_attempts = Int32.succ (Int32.of_float (2. ** 21.)) in
  let alphabet_size = Int32.of_float (2. ** 10.) in
  let expected_mean = 141909l in
  let sigma = 290l in
  let delta = Int32.mul 4l sigma in
  let min_missing, max_missing =
    (Int32.sub expected_mean delta, Int32.add expected_mean delta)
  in
  let generated_words =
    Hashtbl.create
      ~random:false
      (* divide by 8 *)
      (Int32.to_int (Int32.shift_right_logical max_attempts 3))
  in
  let bytes_to_int32 ((b1, b2, b3, b4) : char * char * char * char) =
    let code c = Int32.of_int (Char.code c) in
    let ( + ) = Int32.add in
    let ( * ) = Int32.mul in
    (code b4 * 256l * 256l * 256l)
    + (code b3 * 256l * 256l)
    + (code b2 * 256l)
    + code b1
  in
  let trial (acc : opso_acc_t) (random_4_bytes : char * char * char * char) =
    let random_num = bytes_to_int32 random_4_bytes in
    let keystroke1 = first_letter_selector random_num in
    let keystroke2 = second_letter_selector random_num in
    let word = Int32.add (Int32.mul keystroke1 alphabet_size) keystroke2 in
    Hashtbl.add generated_words word 1l ;
    let attempts = Int32.succ acc.attempts in
    ({ attempts }, if attempts = max_attempts then `Stop else `Continue)
  in
  let () = Iter.(random_seq |> fold_while trial { attempts = 0l } |> ignore) in
  let missing_words =
    Iter.(
      (* 0 -- n^2-1 *)
      (0 -- Int32.(to_int (pred (mul alphabet_size alphabet_size))))
      |> map (fun word ->
             if Hashtbl.mem generated_words (Int32.of_int word) then 0l else 1l )
      |> fold Int32.add 0l)
  in
  Fmt.pr
    "%s: missing_words=%ld | min_missing=%ld | max_missing=%ld@\n"
    name
    missing_words
    min_missing
    max_missing ;
  missing_words >= min_missing && missing_words <= max_missing


let random12Bytes_s : (char * char * char * char) Iter.t =
  let id = M.of_string "random12Bytes" in
  Iter.(
    append_l
      [ singleton (0, 1, 2, 3)
      ; singleton (4, 5, 6, 7)
      ; singleton (8, 9, 10, 11)
      ]
    (* take each pair of byte positions, create random bytes, and
       then throw away all bytes except the four at specified positions.
       that ensure we are testing each position in the byte array *)
    |> map (fun (idx1, idx2, idx3, idx4) ->
           let random_bytes = M.Crypto.random12Bytes id in
           let b1 = M.char_of_elem (M.elem_at random_bytes idx1) in
           let b2 = M.char_of_elem (M.elem_at random_bytes idx2) in
           let b3 = M.char_of_elem (M.elem_at random_bytes idx3) in
           let b4 = M.char_of_elem (M.elem_at random_bytes idx4) in
           (b1, b2, b3, b4) )
    (* infinitely generate new random sequences *)
    |> cycle)


let random32Bytes_s =
  let id = M.of_string "random32Bytes" in
  Iter.(
    append_l
      [ singleton (0, 1, 2, 3)
      ; singleton (4, 5, 6, 7)
      ; singleton (8, 9, 10, 11)
      ; singleton (12, 13, 14, 15)
      ; singleton (16, 17, 18, 19)
      ; singleton (20, 21, 22, 23)
      ; singleton (24, 25, 26, 27)
      ; singleton (28, 29, 30, 31)
      ]
    |> map (fun (idx1, idx2, idx3, idx4) ->
           let random_bytes = M.Crypto.random32Bytes id in
           let b1 = M.char_of_elem (M.elem_at random_bytes idx1) in
           let b2 = M.char_of_elem (M.elem_at random_bytes idx2) in
           let b3 = M.char_of_elem (M.elem_at random_bytes idx3) in
           let b4 = M.char_of_elem (M.elem_at random_bytes idx4) in
           (b1, b2, b3, b4) )
    |> cycle)


let randomBadBytes_s =
  let id = M.of_string "randomBadBytes" in
  Iter.(
    append_l
      [ singleton (0, 1, 2, 3)
      ; singleton (4, 5, 6, 7)
      ; singleton (8, 9, 10, 11)
      ]
    |> map (fun (idx1, _idx2, idx3, _idx4) ->
           let random_bytes = M.Crypto.random12Bytes id in
           (* Clone and XOR some of the bytes. So this is no longer random! *)
           let b1 = M.char_of_elem (M.elem_at random_bytes idx1) in
           let b2 = char_of_int (int_of_char b1 lxor 0b10101010) in
           let b3 = M.char_of_elem (M.elem_at random_bytes idx3) in
           let b4 = char_of_int (int_of_char b3 lxor 0b10101010) in
           (b1, b2, b3, b4) )
    |> cycle)


let test_given_random12Bytes_when_opso_bitrange_1_3_then_pass () =
  Alcotest.(check bool)
    "true"
    true
    (opso_test
       "random12Bytes bits 0-9,20-29"
       random12Bytes_s
       ten_bits_1
       ten_bits_3 )


let test_given_random12Bytes_when_opso_bitrange_2_4_then_pass () =
  Alcotest.(check bool)
    "true"
    true
    (opso_test
       "random12Bytes bits 10-19,22-31"
       random12Bytes_s
       ten_bits_2
       ten_bits_4 )


let test_given_random32Bytes_when_opso_bitrange_1_3_then_pass () =
  Alcotest.(check bool)
    "true"
    true
    (opso_test
       "random32Bytes bits 0-9,20-29"
       random32Bytes_s
       ten_bits_1
       ten_bits_3 )


let test_given_random32Bytes_when_opso_bitrange_2_4_then_pass () =
  Alcotest.(check bool)
    "true"
    true
    (opso_test
       "random32Bytes bits 10-19,22-31"
       random32Bytes_s
       ten_bits_2
       ten_bits_4 )


let test_given_randomBadBytes_when_opso_bitrange_1_3_then_fail () =
  (* we expect this to fail! *)
  Alcotest.(check bool)
    "false"
    false
    (opso_test
       "randomBadBytes bits 0-9,20-29"
       randomBadBytes_s
       ten_bits_1
       ten_bits_3 )


let () =
  let open Alcotest in
  run
    "Random"
    [ ( "ten-bits"
      , [ test_case "First ten bits" `Quick test_first_ten_bits
        ; test_case "Second ten bits" `Quick test_second_ten_bits
        ; test_case "Third ten bits" `Quick test_third_ten_bits
        ; test_case "Last ten bits" `Quick test_last_ten_bits
        ] )
    ; ( "random12Bytes"
      , [ test_case
            "Opso bits  0-9,20-29"
            `Slow
            test_given_random12Bytes_when_opso_bitrange_1_3_then_pass
        ; test_case
            "Opso bits 10-19,22-31"
            `Slow
            test_given_random12Bytes_when_opso_bitrange_2_4_then_pass
        ] )
    ; ( "random32Bytes"
      , [ test_case
            "Opso bits  0-9,20-29"
            `Slow
            test_given_random32Bytes_when_opso_bitrange_1_3_then_pass
        ; test_case
            "Opso bits 10-19,22-31"
            `Slow
            test_given_random32Bytes_when_opso_bitrange_2_4_then_pass
        ] )
    ; ( "randomBadBytes"
      , [ test_case
            "Opso bits  0-9,20-29"
            `Slow
            test_given_randomBadBytes_when_opso_bitrange_1_3_then_fail
        ] )
    ]
