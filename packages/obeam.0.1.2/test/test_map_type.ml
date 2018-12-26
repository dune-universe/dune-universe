open Test_util

let%expect_test "test_map_type.beam" =
  print_ast "test_map_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_map_type.erl 1)
          (AttrMod 1 test_map_type)
          (AttrExportType 3 (
            (any_map 0)
            (ab_map  0)))
          (DeclType 6 any_map () (TyAnyMap 6))
          (DeclType 9 ab_map
            ()
            (TyMap 9 (
              (TyAssocExact 9 (TyLit (LitAtom 9 a)) (TyPredef 9 integer ()))
              (TyAssoc 9 (TyLit (LitAtom 9 b)) (TyPredef 9 atom ())))))
          FormEof)))) |}]
