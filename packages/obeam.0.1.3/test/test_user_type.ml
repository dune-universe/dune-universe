open Test_util

let%expect_test "test_user_type.beam" =
  print_ast "test_user_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_user_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_user_type))
          (AttrExportType (line 3) (type_arity_list ((b 0))))
          (DeclType
            (line 6)
            (name a)
            (tvars ())
            (ty (
              TyPredef
              (line 6)
              (name term)
              (args ()))))
          (DeclType
            (line 7)
            (name b)
            (tvars ())
            (ty (
              TyUser
              (line 7)
              (name a)
              (args ()))))
          FormEof)))) |}]
