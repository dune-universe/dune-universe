open Test_util

let%expect_test "test_operator_type.beam" =
  print_ast "test_operator_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_operator_type.erl 1)
          (AttrMod 1 test_operator_type)
          (AttrExportType 3 (
            (t 0)
            (u 0)
            (v 0)))
          (DeclType 6 t
            ()
            (TyBinOp 6 (TyLit (LitInteger 6 1)) + (TyLit (LitInteger 6 2))))
          (DeclType 7 u
            ()
            (TyBinOp 7 (TyLit (LitInteger 7 3)) * (TyLit (LitInteger 7 4))))
          (DeclType 8 v () (TyUnaryOp 8 - (TyLit (LitInteger 8 4))))
          FormEof)))) |}]
