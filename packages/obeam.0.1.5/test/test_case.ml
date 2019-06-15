open Test_util

let%expect_test "test_case.beam" =
  print_ast "test_case.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_case.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_case))
          (AttrExport (line 3) (function_arity_list ((f 1))))
          (DeclFun
            (line          6)
            (function_name f)
            (arity         1)
            (clauses ((
              ClsFun
              (line 6)
              (patterns ((
                PatVar
                (line 6)
                (id   R))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprCase
                    (line 7)
                    (expr (
                      ExprVar
                      (line 7)
                      (id   R)))
                    (clauses (
                      (ClsCase
                        (line 8)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 8)
                              (atom ok)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    8)
                                  (integer 1)))))))))
                      (ClsCase
                        (line 9)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 9)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    9)
                                  (integer 2))))))))))))))))))))
          FormEof)))) |}]
