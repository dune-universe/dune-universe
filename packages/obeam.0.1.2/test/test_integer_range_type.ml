open Test_util

let%expect_test "test_integer_range_type.beam" =
  print_ast "test_integer_range_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_integer_range_type.erl 1)
          (AttrMod 1 test_integer_range_type)
          (AttrExportType 3 ((t 0)))
          (DeclType 6 t
            ()
            (TyRange 6
              (TyLit (LitInteger 6 1))
              (TyLit (LitInteger 6 2))))
          FormEof)))) |}]
