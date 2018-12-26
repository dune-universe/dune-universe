open Test_util

let%expect_test "test_type_attr.beam" =
  print_ast "test_type_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_type_attr.erl 1)
          (AttrMod 1 test_type_attr)
          (AttrExportType 3 (
            (tuple 2)
            (int   0)))
          (DeclType 6 tuple
            ((6 A)
             (6 B))
            (TyTuple 6 (
              (TyVar 6 A)
              (TyVar 6 B))))
          (DeclOpaqueType 9 int () (TyPredef 9 integer ()))
          FormEof)))) |}]
