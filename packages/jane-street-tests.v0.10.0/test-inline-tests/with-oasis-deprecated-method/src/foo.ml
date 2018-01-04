let%test "test1" = true
let%test "test2" = false

let%expect_test "expect test1" =
  print_string "foo";
  [%expect {|foo|}]

let%expect_test "expect test2" =
  print_string "bar";
  [%expect {|XXX|}]
