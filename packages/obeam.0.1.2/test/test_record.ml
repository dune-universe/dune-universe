open Test_util

let%expect_test "test_record.beam" =
  print_ast "test_record.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_record.erl 1)
          (AttrMod 1 test_record)
          (DeclRecord 4 (
            (4 a
              ()
              ())
            (5 b ((ExprLit (LitInteger 5 42))) ())
            (6 c () ((TyPredef 6 string ())))
            (7 d ((ExprLit (LitInteger 7 57))) ((TyPredef 7 integer ())))))
          FormEof)))) |}]
