open Test_util

let%expect_test "test_try.beam" =
  print_ast "test_try.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_try.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_try))
          (AttrExport
            (line 3)
            (function_arity_list (
              (try_catch          0)
              (try_of_catch       0)
              (try_after          0)
              (try_of_after       0)
              (try_catch_after    0)
              (try_of_catch_after 0))))
          (DeclFun
            (line          7)
            (function_name try_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 7)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 8)
                    (exprs (
                      (ExprMatch
                        (line 9)
                        (pattern (
                          PatVar
                          (line 9)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 9)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 10)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 10)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 10)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            12)
                        (line_cls        12)
                        (line_stacktrace 12)
                        (exception_class (AtomVarAtom (line 12) (atom throw)))
                        (pattern         (PatVar      (line 12) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 12)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 12)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 12)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 12)
                              (id   Err)))))))
                      (ClsCatch
                        (line            13)
                        (line_cls        13)
                        (line_stacktrace 13)
                        (exception_class (AtomVarAtom (line 13) (atom error)))
                        (pattern         (PatVar      (line 13) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 13)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 13)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 13)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 13)
                              (id   Err)))))))
                      (ClsCatch
                        (line            14)
                        (line_cls        14)
                        (line_stacktrace 14)
                        (exception_class (AtomVarAtom (line 14) (atom throw)))
                        (pattern         (PatVar      (line 14) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 14)
                              (id   Err)))))))
                      (ClsCatch
                        (line            15)
                        (line_cls        15)
                        (line_stacktrace 15)
                        (exception_class (AtomVarAtom (line 15) (atom error)))
                        (pattern         (PatVar      (line 15) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 15)
                              (id   Err)))))))))
                    (after ()))))))))))
          (DeclFun
            (line          19)
            (function_name try_of_catch)
            (arity         0)
            (clauses ((
              ClsFun
              (line 19)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 20)
                    (exprs (
                      (ExprMatch
                        (line 21)
                        (pattern (
                          PatVar
                          (line 21)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 21)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 22)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 22)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 22)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 24)
                        (pattern (
                          PatTuple
                          (line 24)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 24)
                                (atom ok))))
                            (PatVar
                              (line 24)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 24)
                              (id   A)))))))
                      (ClsCase
                        (line 25)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 25)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 25)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            27)
                        (line_cls        27)
                        (line_stacktrace 27)
                        (exception_class (AtomVarAtom (line 27) (atom throw)))
                        (pattern         (PatVar      (line 27) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 27)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 27)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 27)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 27)
                              (id   Err)))))))
                      (ClsCatch
                        (line            28)
                        (line_cls        28)
                        (line_stacktrace 28)
                        (exception_class (AtomVarAtom (line 28) (atom error)))
                        (pattern         (PatVar      (line 28) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 28)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 28)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 28)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 28)
                              (id   Err)))))))
                      (ClsCatch
                        (line            29)
                        (line_cls        29)
                        (line_stacktrace 29)
                        (exception_class (AtomVarAtom (line 29) (atom throw)))
                        (pattern         (PatVar      (line 29) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 29)
                              (id   Err)))))))
                      (ClsCatch
                        (line            30)
                        (line_cls        30)
                        (line_stacktrace 30)
                        (exception_class (AtomVarAtom (line 30) (atom error)))
                        (pattern         (PatVar      (line 30) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 30)
                              (id   Err)))))))))
                    (after ()))))))))))
          (DeclFun
            (line          34)
            (function_name try_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 34)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 35)
                    (exprs (
                      (ExprMatch
                        (line 36)
                        (pattern (
                          PatVar
                          (line 36)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 36)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 37)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 37)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 37)
                          (id   R)))))))
                    (case_clauses  ())
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 39)
                        (pattern (
                          PatVar
                          (line 39)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 39)
                              (atom ok))))))
                      (ExprVar
                        (line 40)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          44)
            (function_name try_of_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 44)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 45)
                    (exprs (
                      (ExprMatch
                        (line 46)
                        (pattern (
                          PatVar
                          (line 46)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 46)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 47)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 47)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 47)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 49)
                        (pattern (
                          PatTuple
                          (line 49)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 49)
                                (atom ok))))
                            (PatVar
                              (line 49)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 49)
                              (id   A)))))))
                      (ClsCase
                        (line 50)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 50)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 50)
                                  (atom error)))))))))))
                    (catch_clauses ())
                    (after (
                      (ExprMatch
                        (line 52)
                        (pattern (
                          PatVar
                          (line 52)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 52)
                              (atom ok))))))
                      (ExprVar
                        (line 53)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          57)
            (function_name try_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 57)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 58)
                    (exprs (
                      (ExprMatch
                        (line 59)
                        (pattern (
                          PatVar
                          (line 59)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 59)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 60)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 60)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 60)
                          (id   R)))))))
                    (case_clauses ())
                    (catch_clauses (
                      (ClsCatch
                        (line            62)
                        (line_cls        62)
                        (line_stacktrace 62)
                        (exception_class (AtomVarAtom (line 62) (atom throw)))
                        (pattern         (PatVar      (line 62) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 62)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 62)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 62)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 62)
                              (id   Err)))))))
                      (ClsCatch
                        (line            63)
                        (line_cls        63)
                        (line_stacktrace 63)
                        (exception_class (AtomVarAtom (line 63) (atom error)))
                        (pattern         (PatVar      (line 63) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 63)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 63)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 63)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 63)
                              (id   Err)))))))
                      (ClsCatch
                        (line            64)
                        (line_cls        64)
                        (line_stacktrace 64)
                        (exception_class (AtomVarAtom (line 64) (atom throw)))
                        (pattern         (PatVar      (line 64) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 64)
                              (id   Err)))))))
                      (ClsCatch
                        (line            65)
                        (line_cls        65)
                        (line_stacktrace 65)
                        (exception_class (AtomVarAtom (line 65) (atom error)))
                        (pattern         (PatVar      (line 65) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 65)
                              (id   Err)))))))))
                    (after (
                      (ExprMatch
                        (line 67)
                        (pattern (
                          PatVar
                          (line 67)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 67)
                              (atom ok))))))
                      (ExprVar
                        (line 68)
                        (id   Ok)))))))))))))
          (DeclFun
            (line          72)
            (function_name try_of_catch_after)
            (arity         0)
            (clauses ((
              ClsFun
              (line 72)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTry
                    (line 73)
                    (exprs (
                      (ExprMatch
                        (line 74)
                        (pattern (
                          PatVar
                          (line 74)
                          (id   R)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 74)
                              (atom reason))))))
                      (ExprLocalCall
                        (line 75)
                        (function_expr (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 75)
                              (atom error)))))
                        (args ((
                          ExprVar
                          (line 75)
                          (id   R)))))))
                    (case_clauses (
                      (ClsCase
                        (line 77)
                        (pattern (
                          PatTuple
                          (line 77)
                          (pats (
                            (PatLit (
                              lit (
                                LitAtom
                                (line 77)
                                (atom ok))))
                            (PatVar
                              (line 77)
                              (id   A))))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 77)
                              (id   A)))))))
                      (ClsCase
                        (line 78)
                        (pattern (
                          PatLit (
                            lit (
                              LitAtom
                              (line 78)
                              (atom error)))))
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprLit (
                                lit (
                                  LitAtom
                                  (line 78)
                                  (atom error)))))))))))
                    (catch_clauses (
                      (ClsCatch
                        (line            80)
                        (line_cls        80)
                        (line_stacktrace 80)
                        (exception_class (AtomVarAtom (line 80) (atom throw)))
                        (pattern         (PatVar      (line 80) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 80)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 80)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 80)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 80)
                              (id   Err)))))))
                      (ClsCatch
                        (line            81)
                        (line_cls        81)
                        (line_stacktrace 81)
                        (exception_class (AtomVarAtom (line 81) (atom error)))
                        (pattern         (PatVar      (line 81) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ((
                          GuardSeq (
                            guards ((
                              Guard (
                                guard_tests ((
                                  GuardTestBinOp
                                  (line 81)
                                  (op   =:=)
                                  (lhs (
                                    GuardTestVar
                                    (line 81)
                                    (id   Err)))
                                  (rhs (
                                    GuardTestLit (
                                      lit (
                                        LitAtom
                                        (line 81)
                                        (atom error))))))))))))))
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 81)
                              (id   Err)))))))
                      (ClsCatch
                        (line            82)
                        (line_cls        82)
                        (line_stacktrace 82)
                        (exception_class (AtomVarAtom (line 82) (atom throw)))
                        (pattern         (PatVar      (line 82) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 82)
                              (id   Err)))))))
                      (ClsCatch
                        (line            83)
                        (line_cls        83)
                        (line_stacktrace 83)
                        (exception_class (AtomVarAtom (line 83) (atom error)))
                        (pattern         (PatVar      (line 83) (id   Err)))
                        (stacktrace _)
                        (guard_sequence ())
                        (body (
                          ExprBody (
                            exprs ((
                              ExprVar
                              (line 83)
                              (id   Err)))))))))
                    (after (
                      (ExprMatch
                        (line 85)
                        (pattern (
                          PatVar
                          (line 85)
                          (id   Ok)))
                        (body (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 85)
                              (atom ok))))))
                      (ExprVar
                        (line 86)
                        (id   Ok)))))))))))))
          FormEof)))) |}]
