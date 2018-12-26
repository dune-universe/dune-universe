open Test_util

let%expect_test "test_user_type.beam" =
  print_ast "test_user_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_user_type.erl 1)
          (AttrMod 1 test_user_type)
          (AttrExportType 3 ((b 0)))
          (DeclType 6 a () (TyPredef 6 term ()))
          (DeclType 7 b () (TyUser   7 a    ()))
          FormEof)))) |}]
