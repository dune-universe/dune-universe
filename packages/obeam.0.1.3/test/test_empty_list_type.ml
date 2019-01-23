open Test_util

let%expect_test "test_empty_list_type.beam" =
  print_ast "test_empty_list_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_empty_list_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_empty_list_type))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (t 0)
              (u 0))))
          (DeclType
            (line 6)
            (name t)
            (tvars ())
            (ty (
              TyPredef
              (line 6)
              (name nil)
              (args ()))))
          (DeclType
            (line 7)
            (name u)
            (tvars ())
            (ty (
              TyPredef
              (line 7)
              (name nil)
              (args ()))))
          FormEof)))) |}]
