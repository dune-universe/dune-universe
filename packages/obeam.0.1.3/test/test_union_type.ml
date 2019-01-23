open Test_util

let%expect_test "test_union_type.beam" =
  print_ast "test_union_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_union_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_union_type))
          (AttrExportType (line 3) (type_arity_list ((t 0))))
          (DeclType
            (line 6)
            (name t)
            (tvars ())
            (ty (
              TyUnion
              (line 6)
              (elements (
                (TyPredef (line 6) (name integer) (args ()))
                (TyPredef (line 6) (name string)  (args ())))))))
          FormEof)))) |}]
