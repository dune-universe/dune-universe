open Jhupllib;;
open OUnit2

open String_utils;;

let natural_compare_seq_returns_0_for_empty_list _ =
  assert_equal 0 (Utils.natural_compare_seq [])

let string_of_list_list pp lst =
  string_of_list (string_of_list pp) lst
;;

let cartesian_product_tests =
  let do_test lst1 lst2 _ =
    assert_equal (Utils.cartesian_product_of_list lst1) lst2
  in
  let make_test (lst1,lst2) =
    let test_name = "cartesian_product_of_list " ^
                    string_of_list_list string_of_int lst1 ^ " = " ^
                    string_of_list_list string_of_int lst2
    in
    test_name >:: do_test lst1 lst2
  in
  List.map make_test
    [ ( []
      , [[]]
      )
    ; ( [[1]]
      , [[1]]
      )
    ; ( [[1;2]]
      , [[1];[2]]
      )
    ; ( [[1;2];[3]]
      , [[1;3];[2;3]]
      )
    ; ( [[1;2];[3];[4;5;6]]
      , [[1;3;4];[2;3;4];[1;3;5];[2;3;5];[1;3;6];[2;3;6]]
      )
    ; ( [[1;2];[];[4;5;6]]
      , []
      )
    ]

let tests = "Utils" >::: [
    "natural_compare_seq returns 0 for empty list" >:: natural_compare_seq_returns_0_for_empty_list;
  ] @
    cartesian_product_tests
;;
