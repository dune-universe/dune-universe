open Test_util

let%expect_test "test_funt.beam" =
  print_ast "test_funt.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_funt.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_funt))
          (AttrExport (line 3) (function_arity_list ((g 0))))
          (DeclFun
            (line          5)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 5)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitAtom
                        (line 5)
                        (atom ok))))))))))))
          (DeclFun
            (line          8)
            (function_name g)
            (arity         0)
            (clauses ((
              ClsFun
              (line 8)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLocalFunRef
                    (line          8)
                    (function_name f)
                    (arity         0))))))))))
          FormEof)))) |}]
