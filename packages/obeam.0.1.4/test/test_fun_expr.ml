open Test_util

let%expect_test "test_fun_expr.beam" =
  print_ast "test_fun_expr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_fun_expr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_fun_expr))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 0)
              (h 0))))
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
                    ExprFun
                    (line 7)
                    (name ())
                    (clauses (
                      (ClsFun
                        (line 7)
                        (patterns ((
                          PatLit (
                            lit (
                              LitInteger
                              (line    7)
                              (integer 42))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    7)
                                  (integer 42)))))))))
                      (ClsFun
                        (line 8)
                        (patterns ((PatUniversal (line 8))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    8)
                                  (integer 57))))))))))))))))))))
          (DeclFun
            (line          12)
            (function_name g)
            (arity         0)
            (clauses ((
              ClsFun
              (line 12)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprFun
                    (line 13)
                    (name (F))
                    (clauses (
                      (ClsFun
                        (line 13)
                        (patterns ((
                          PatLit (
                            lit (
                              LitInteger
                              (line    13)
                              (integer 42))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLocalCall
                              (line 13)
                              (function_expr (
                                ExprVar
                                (line 13)
                                (id   F)))
                              (args ((
                                ExprLit (
                                  lit (
                                    LitInteger
                                    (line    13)
                                    (integer 57))))))))))))
                      (ClsFun
                        (line 14)
                        (patterns ((PatUniversal (line 14))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    14)
                                  (integer 57))))))))))))))))))))
          (DeclFun
            (line          18)
            (function_name h)
            (arity         0)
            (clauses ((
              ClsFun
              (line 18)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprFun
                    (line 19)
                    (name ())
                    (clauses (
                      (ClsFun
                        (line 19)
                        (patterns ((
                          PatVar
                          (line 19)
                          (id   N))))
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 19)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 19)
                                    (id   N)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitInteger
                                        (line    19)
                                        (integer 42))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 19)
                              (id   N)))))))
                      (ClsFun
                        (line 20)
                        (patterns ((PatUniversal (line 20))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    20)
                                  (integer 57))))))))))))))))))))
          FormEof)))) |}]
