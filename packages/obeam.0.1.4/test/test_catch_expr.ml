open Test_util

let%expect_test "test_catch_expr.beam" =
  print_ast "test_catch_expr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_catch_expr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_catch_expr))
          (AttrExport (line 3) (function_arity_list ((f 0))))
          (DeclFun
            (line          6)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 6)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprCatch
                    (line 7)
                    (expr (
                      ExprLocalCall
                      (line 7)
                      (function_expr (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 7)
                            (atom error)))))
                      (args ((
                        ExprLit (
                          lit (
                            LitAtom
                            (line 7)
                            (atom foo)))))))))))))))))
          FormEof)))) |}]
