open Test_util

let%expect_test "test_type_attr.beam" =
  print_ast "test_type_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_type_attr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_type_attr))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (tuple 2)
              (int   0))))
          (DeclType
            (line 6)
            (name tuple)
            (tvars (
              (6 A)
              (6 B)))
            (ty (
              TyTuple
              (line 6)
              (elements (
                (TyVar (line 6) (id A))
                (TyVar (line 6) (id B)))))))
          (DeclOpaqueType
            (line 9)
            (name int)
            (tvars ())
            (ty (
              TyPredef
              (line 9)
              (name integer)
              (args ()))))
          FormEof)))) |}]
