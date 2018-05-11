(********************************************************************************)
(*  Test_prelude.ml
    Copyright (c) 2018 Dario Teixeira <dario.teixeira@nleyten.com>
    This software is distributed under the terms of the ISC license.
    See LICENSE file for full license text.
*)
(********************************************************************************)

open Passmaker
open Prelude


(********************************************************************************)
(** {1 Tests for Array module}                                                  *)
(********************************************************************************)

module Test_Array =
struct
    let arr_empty = [| |]
    let arr_one = [| 1 |]
    let arr_two = [| 1; 3 |]
    let arr_four = [| 1; 2; 4; 6 |]

    let test_binary_search () =
        let run desc expected x arr =
            Alcotest.(check (option int) desc expected (Array.binary_search Pervasives.compare x arr)) in
        run "0_in_empty" None 0 arr_empty;
        run "0_in_one" None 0 arr_one;
        run "1_in_one" (Some 0) 1 arr_one;
        run "2_in_one" None 2 arr_one;
        run "0_in_two" None 0 arr_two;
        run "1_in_two" (Some 0) 1 arr_two;
        run "2_in_two" None 2 arr_two;
        run "3_in_two" (Some 1) 3 arr_two;
        run "4_in_two" None 4 arr_two;
        run "0_in_four" None 0 arr_four;
        run "1_in_four" (Some 0) 1 arr_four;
        run "2_in_four" (Some 1) 2 arr_four;
        run "3_in_four" None 3 arr_four;
        run "4_in_four" (Some 2) 4 arr_four;
        run "5_in_four" None 5 arr_four;
        run "6_in_four" (Some 3) 6 arr_four;
        run "7_in_four" None 7 arr_four

    let testset =
        [
        ("binary_search", `Quick, test_binary_search);
        ]
end


(********************************************************************************)
(** {1 Tests for String module}                                                 *)
(********************************************************************************)

module Test_String =
struct
    let test_edit_distance () =
        let run desc expected a b =
            Alcotest.(check int desc expected (String.edit_distance a b)) in
        run "identical0" 0 "" "";
        run "identical1" 0 "a" "a";
        run "identical2" 0 "ab" "ab";
        run "substitution1" 1 "abcdef" "abxdef";
        run "substitution2" 2 "abcdef" "axcdyf";
        run "substitution3" 3 "abcdef" "xbydez";
        run "insertion1" 1 "abcd" "abxcd";
        run "insertion2" 2 "abcd" "abxycd";
        run "insertion3" 3 "abcd" "abxyzcd";
        run "deletion1" 1 "abcdef" "abdef";
        run "deletion2" 2 "abcdef" "abef";
        run "deletion3" 3 "abcdef" "abf";
        run "transposition1" 1 "abcdef" "abdcef"

    let testset =
        [
        ("edit_distance", `Quick, test_edit_distance);
        ]
end


(********************************************************************************)
(** {1 Main}                                                                    *)
(********************************************************************************)

let () = Alcotest.run "Prelude module"
    [
    ("Array", Test_Array.testset);
    ("String", Test_String.testset);
    ]
