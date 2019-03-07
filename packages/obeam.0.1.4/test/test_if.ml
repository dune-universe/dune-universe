open Test_util

let%expect_test "test_if.beam" =
  print_ast "test_if.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_if.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_if))
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
                (id   X))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprIf
                    (line 7)
                    (clauses (
                      (ClsIf
                        (line 8)
                        (guard_sequence (
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 8)
                                  (op   >)
                                  (lhs (
                                    GuardTestVar
                                    (line 8)
                                    (id   X)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitInteger
                                        (line    8)
                                        (integer 0)))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 8)
                                  (atom hoge)))))))))
                      (ClsIf
                        (line 9)
                        (guard_sequence (
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestLit (
                                    lit (
                                      LitAtom
                                      (line 9)
                                      (atom true)))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 9)
                                  (atom piyo))))))))))))))))))))
          FormEof)))) |}]
