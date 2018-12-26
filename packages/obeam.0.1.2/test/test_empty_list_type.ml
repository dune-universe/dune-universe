open Test_util

let%expect_test "test_empty_list_type.beam" =
  print_ast "test_empty_list_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_empty_list_type.erl 1)
          (AttrMod 1 test_empty_list_type)
          (AttrExportType 3 (
            (t 0)
            (u 0)))
          (DeclType 6 t () (TyPredef 6 nil ()))
          (DeclType 7 u () (TyPredef 7 nil ()))
          FormEof)))) |}]
