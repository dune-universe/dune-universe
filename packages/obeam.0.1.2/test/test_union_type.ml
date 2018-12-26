open Test_util

let%expect_test "test_union_type.beam" =
  print_ast "test_union_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_union_type.erl 1)
          (AttrMod 1 test_union_type)
          (AttrExportType 3 ((t 0)))
          (DeclType 6 t
            ()
            (TyUnion 6 (
              (TyPredef 6 integer ())
              (TyPredef 6 string  ()))))
          FormEof)))) |}]
