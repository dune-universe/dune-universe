open Test_util

let%expect_test "test_fun_type.beam" =
  print_ast "test_fun_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun_type.erl 1)
          (AttrMod 1 test_fun_type)
          (AttrExportType 3 (
            (t 0)
            (u 0)
            (v 0)
            (w 0)))
          (DeclType 6 t () (TyFunAny 6))
          (DeclType 7 u () (TyFunAnyArity 7 7 (TyPredef 7 integer ())))
          (DeclType 8 v () (TyFun 8 8 () (TyPredef 8 integer ())))
          (DeclType 9 w
            ()
            (TyFun 9 9 ((TyPredef 9 integer ())) (TyPredef 9 integer ())))
          FormEof)))) |}]
