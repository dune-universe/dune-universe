open OUnit2
open Base


let equality_tests = 
[
  ("Issue #1 : Modified range can be equal to a natural one" >:: fun _ctxt ->
    let data_a = Range.from 3 6 in
    let data_b = Range.from 2 5 |> Range.map Int.succ in
    assert_bool "equality failed" (Range.equal data_a data_b)
  )
]

let non_reg_tests = [
    equality_tests
] |> List.concat


let () = 
  "Range non regression bug testing" >::: non_reg_tests |> run_test_tt_main
