open Test_util

let%expect_test "test_fun_call.beam" =
  print_ast "test_fun_call.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_fun_call.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_fun_call))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (f 1)
              (f 2))))
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
                  exprs (
                    (ExprMatch
                      (line 7)
                      (pattern (
                        PatVar
                        (line 7)
                        (id   F)))
                      (body (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 7)
                            (atom f))))))
                    (ExprLocalCall
                      (line 9)
                      (function_expr (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 9)
                            (atom f)))))
                      (args ((
                        ExprLit (
                          lit (
                            LitInteger
                            (line    9)
                            (integer 1)))))))
                    (ExprLocalCall
                      (line 10)
                      (function_expr (
                        ExprVar
                        (line 10)
                        (id   F)))
                      (args ((
                        ExprLit (
                          lit (
                            LitInteger
                            (line    10)
                            (integer 1)))))))
                    (ExprLocalCall
                      (line 11)
                      (function_expr (
                        ExprCase
                        (line 11)
                        (expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 11)
                              (atom true)))))
                        (clauses ((
                          ClsCase
                          (line 11)
                          (pattern (PatUniversal (line 11)))
                          (guard_sequence ())
                          (body (
                            ExprBody (
                              exprs ((
                                ExprVar
                                (line 11)
                                (id   F)))))))))))
                      (args ((
                        ExprLit (
                          lit (
                            LitInteger
                            (line    11)
                            (integer 1)))))))))))))))
          (DeclFun
            (line          14)
            (function_name f)
            (arity         1)
            (clauses ((
              ClsFun
              (line 14)
              (patterns ((
                PatVar
                (line 14)
                (id   N))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 15)
                      (pattern (
                        PatVar
                        (line 15)
                        (id   M)))
                      (body (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 15)
                            (atom test_fun_call))))))
                    (ExprMatch
                      (line 16)
                      (pattern (
                        PatVar
                        (line 16)
                        (id   F)))
                      (body (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 16)
                            (atom f))))))
                    (ExprRemoteCall
                      (line        18)
                      (line_remote 18)
                      (module_expr (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 18)
                            (atom test_fun_call)))))
                      (function_expr (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 18)
                            (atom f)))))
                      (args (
                        (ExprVar
                          (line 18)
                          (id   N))
                        (ExprLit (
                          lit (
                            LitInteger
                            (line    18)
                            (integer 2)))))))
                    (ExprRemoteCall
                      (line        19)
                      (line_remote 19)
                      (module_expr   (ExprVar (line 19) (id M)))
                      (function_expr (ExprVar (line 19) (id F)))
                      (args (
                        (ExprVar
                          (line 19)
                          (id   N))
                        (ExprLit (
                          lit (
                            LitInteger
                            (line    19)
                            (integer 2)))))))
                    (ExprRemoteCall
                      (line        20)
                      (line_remote 20)
                      (module_expr (
                        ExprCase
                        (line 20)
                        (expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 20)
                              (atom true)))))
                        (clauses ((
                          ClsCase
                          (line 20)
                          (pattern (PatUniversal (line 20)))
                          (guard_sequence ())
                          (body (
                            ExprBody (
                              exprs ((
                                ExprVar
                                (line 20)
                                (id   M)))))))))))
                      (function_expr (
                        ExprCase
                        (line 20)
                        (expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 20)
                              (atom true)))))
                        (clauses ((
                          ClsCase
                          (line 20)
                          (pattern (PatUniversal (line 20)))
                          (guard_sequence ())
                          (body (
                            ExprBody (
                              exprs ((
                                ExprVar
                                (line 20)
                                (id   F)))))))))))
                      (args (
                        (ExprVar
                          (line 20)
                          (id   N))
                        (ExprLit (
                          lit (
                            LitInteger
                            (line    20)
                            (integer 2)))))))))))))))
          (DeclFun
            (line          22)
            (function_name f)
            (arity         2)
            (clauses ((
              ClsFun
              (line 22)
              (patterns (
                (PatVar (line 22) (id N))
                (PatVar (line 22) (id M))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprBinOp
                    (line 22)
                    (op   +)
                    (lhs (ExprVar (line 22) (id N)))
                    (rhs (ExprVar (line 22) (id M))))))))))))
          FormEof)))) |}]
