open Test_util

let%expect_test "test_integer_range_type.beam" =
  print_ast "test_integer_range_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_integer_range_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_integer_range_type))
          (AttrExportType (line 3) (type_arity_list ((t 0))))
          (DeclType
            (line 6)
            (name t)
            (tvars ())
            (ty (
              TyRange
              (line 6)
              (low (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 1)))))
              (high (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 2))))))))
          FormEof)))) |}]
