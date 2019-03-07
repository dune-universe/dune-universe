open Test_util

let%expect_test "test_bitstring.beam" =
  print_ast "test_bitstring.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_bitstring.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_bitstring))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 1)
              (h 1)
              (i 0))))
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
                    ExprBitstr
                    (line 7)
                    (elements (
                      (ExprBinElement
                        (expr (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    7)
                              (integer 0)))))
                        (size ())
                        (tsl  ()))
                      (ExprBinElement
                        (expr (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    8)
                              (integer 1)))))
                        (size ((
                          ExprLit (
                            lit (
                              LitInteger
                              (line    8)
                              (integer 1))))))
                        (tsl ()))
                      (ExprBinElement
                        (expr (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    9)
                              (integer 2)))))
                        (size ())
                        (tsl ((
                          (TypeSpec (atom little)  (value ()))
                          (TypeSpec (atom signed)  (value ()))
                          (TypeSpec (atom integer) (value ()))))))
                      (ExprBinElement
                        (expr (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    10)
                              (integer 3)))))
                        (size ((
                          ExprLit (
                            lit (
                              LitInteger
                              (line    10)
                              (integer 3))))))
                        (tsl ((
                          (TypeSpec (atom little) (value ()))
                          (TypeSpec (atom signed) (value ()))
                          (TypeSpec (atom unit) (value (8)))
                          (TypeSpec (atom integer) (value ()))))))
                      (ExprBinElement
                        (expr (
                          ExprBinOp
                          (line 11)
                          (op   +)
                          (lhs (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    11)
                                (integer 2)))))
                          (rhs (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    11)
                                (integer 2)))))))
                        (size ((
                          ExprBinOp
                          (line 11)
                          (op   +)
                          (lhs (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    11)
                                (integer 2)))))
                          (rhs (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    11)
                                (integer 2))))))))
                        (tsl ())))))))))))))
          (DeclFun
            (line          15)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 15)
              (patterns ((
                PatBitstr
                (line 15)
                (elements (
                  (PatBinElement
                    (pattern (
                      PatLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 0)))))
                    (size ())
                    (tsl  ()))
                  (PatBinElement
                    (pattern (
                      PatLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 1)))))
                    (size ((
                      ExprLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 1))))))
                    (tsl ()))
                  (PatBinElement
                    (pattern (
                      PatLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 2)))))
                    (size ())
                    (tsl ((
                      (TypeSpec (atom little)  (value ()))
                      (TypeSpec (atom signed)  (value ()))
                      (TypeSpec (atom integer) (value ()))))))
                  (PatBinElement
                    (pattern (
                      PatLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 3)))))
                    (size ((
                      ExprLit (
                        lit (
                          LitInteger
                          (line    15)
                          (integer 3))))))
                    (tsl ((
                      (TypeSpec (atom little) (value ()))
                      (TypeSpec (atom signed) (value ()))
                      (TypeSpec (atom unit) (value (8)))
                      (TypeSpec (atom integer) (value ()))))))
                  (PatBinElement
                    (pattern (
                      PatVar
                      (line 15)
                      (id   L)))
                    (size ((
                      ExprBinOp
                      (line 15)
                      (op   +)
                      (lhs (
                        ExprLit (
                          lit (
                            LitInteger
                            (line    15)
                            (integer 2)))))
                      (rhs (
                        ExprLit (
                          lit (
                            LitInteger
                            (line    15)
                            (integer 2))))))))
                    (tsl ()))
                  (PatBinElement
                    (pattern (
                      PatVar
                      (line 15)
                      (id   B)))
                    (size ((
                      ExprVar
                      (line 15)
                      (id   L))))
                    (tsl ())))))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprVar
                    (line 15)
                    (id   B))))))))))
          (DeclFun
            (line          18)
            (function_name h)
            (arity         1)
            (clauses ((
              ClsFun
              (line 18)
              (patterns ((
                PatVar
                (line 18)
                (id   B))))
              (guard_sequence ((
                GuardSeq (
                  guards ((
                    Guard (
                      guard_tests ((
                        GuardTestBinOp
                        (line 18)
                        (op   =:=)
                        (lhs (
                          GuardTestVar
                          (line 18)
                          (id   B)))
                        (rhs (
                          GuardTestBitstr
                          (line 18)
                          (elements (
                            (GuardTestBinElement
                              (guard_test (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 0)))))
                              (size ())
                              (tsl  ()))
                            (GuardTestBinElement
                              (guard_test (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 1)))))
                              (size ((
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 1))))))
                              (tsl ()))
                            (GuardTestBinElement
                              (guard_test (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 2)))))
                              (size ())
                              (tsl ((
                                (TypeSpec (atom little)  (value ()))
                                (TypeSpec (atom signed)  (value ()))
                                (TypeSpec (atom integer) (value ()))))))
                            (GuardTestBinElement
                              (guard_test (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 3)))))
                              (size ((
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    18)
                                    (integer 3))))))
                              (tsl ((
                                (TypeSpec (atom little) (value ()))
                                (TypeSpec (atom signed) (value ()))
                                (TypeSpec (atom unit) (value (8)))
                                (TypeSpec (atom integer) (value ()))))))
                            (GuardTestBinElement
                              (guard_test (
                                GuardTestBinOp
                                (line 18)
                                (op   +)
                                (lhs (
                                  GuardTestLit (
                                    lit (
                                      LitInteger
                                      (line    18)
                                      (integer 2)))))
                                (rhs (
                                  GuardTestLit (
                                    lit (
                                      LitInteger
                                      (line    18)
                                      (integer 2)))))))
                              (size ((
                                GuardTestBinOp
                                (line 18)
                                (op   +)
                                (lhs (
                                  GuardTestLit (
                                    lit (
                                      LitInteger
                                      (line    18)
                                      (integer 2)))))
                                (rhs (
                                  GuardTestLit (
                                    lit (
                                      LitInteger
                                      (line    18)
                                      (integer 2))))))))
                              (tsl ())))))))))))))))
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitAtom
                        (line 18)
                        (atom ok))))))))))))
          (DeclFun
            (line          21)
            (function_name i)
            (arity         0)
            (clauses ((
              ClsFun
              (line 21)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 22)
                      (pattern (PatUniversal (line 22)))
                      (body (
                        ExprBitstrComprehension
                        (line 22)
                        (expr (
                          ExprBitstr
                          (line 22)
                          (elements ((
                            ExprBinElement
                            (expr (
                              ExprVar
                              (line 22)
                              (id   X)))
                            (size ())
                            (tsl  ()))))))
                        (qualifiers (
                          (QualifierBitstrGenerator
                            (line 22)
                            (pattern (
                              PatBitstr
                              (line 22)
                              (elements ((
                                PatBinElement
                                (pattern (
                                  PatVar
                                  (line 22)
                                  (id   X)))
                                (size ())
                                (tsl  ()))))))
                            (expr (
                              ExprBitstr
                              (line 22)
                              (elements (
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    22)
                                        (integer 1)))))
                                  (size ())
                                  (tsl  ()))
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    22)
                                        (integer 2)))))
                                  (size ())
                                  (tsl  ()))
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    22)
                                        (integer 3)))))
                                  (size ())
                                  (tsl  ())))))))
                          (QualifierFilter (
                            filter (
                              ExprBinOp
                              (line 22)
                              (op   >=)
                              (lhs (
                                ExprVar
                                (line 22)
                                (id   X)))
                              (rhs (
                                ExprLit (
                                  lit (
                                    LitInteger
                                    (line    22)
                                    (integer 2)))))))))))))
                    (ExprMatch
                      (line 23)
                      (pattern (PatUniversal (line 23)))
                      (body (
                        ExprBitstrComprehension
                        (line 23)
                        (expr (
                          ExprBitstr
                          (line 23)
                          (elements ((
                            ExprBinElement
                            (expr (
                              ExprVar
                              (line 23)
                              (id   X)))
                            (size ())
                            (tsl  ()))))))
                        (qualifiers (
                          (QualifierGenerator
                            (line 23)
                            (pattern (
                              PatVar
                              (line 23)
                              (id   X)))
                            (expr (
                              ExprCons
                              (line 23)
                              (head (
                                ExprLit (
                                  lit (
                                    LitInteger
                                    (line    23)
                                    (integer 1)))))
                              (tail (
                                ExprCons
                                (line 23)
                                (head (
                                  ExprLit (
                                    lit (
                                      LitInteger
                                      (line    23)
                                      (integer 2)))))
                                (tail (
                                  ExprCons
                                  (line 23)
                                  (head (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    23)
                                        (integer 3)))))
                                  (tail (ExprNil (line 23))))))))))
                          (QualifierFilter (
                            filter (
                              ExprBinOp
                              (line 23)
                              (op   >=)
                              (lhs (
                                ExprVar
                                (line 23)
                                (id   X)))
                              (rhs (
                                ExprLit (
                                  lit (
                                    LitInteger
                                    (line    23)
                                    (integer 2)))))))))))))
                    (ExprMatch
                      (line 24)
                      (pattern (PatUniversal (line 24)))
                      (body (
                        ExprListComprehension
                        (line 24)
                        (expr (
                          ExprVar
                          (line 24)
                          (id   X)))
                        (qualifiers (
                          (QualifierBitstrGenerator
                            (line 24)
                            (pattern (
                              PatBitstr
                              (line 24)
                              (elements ((
                                PatBinElement
                                (pattern (
                                  PatVar
                                  (line 24)
                                  (id   X)))
                                (size ())
                                (tsl  ()))))))
                            (expr (
                              ExprBitstr
                              (line 24)
                              (elements (
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    24)
                                        (integer 1)))))
                                  (size ())
                                  (tsl  ()))
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    24)
                                        (integer 2)))))
                                  (size ())
                                  (tsl  ()))
                                (ExprBinElement
                                  (expr (
                                    ExprLit (
                                      lit (
                                        LitInteger
                                        (line    24)
                                        (integer 3)))))
                                  (size ())
                                  (tsl  ())))))))
                          (QualifierFilter (
                            filter (
                              ExprBinOp
                              (line 24)
                              (op   >=)
                              (lhs (
                                ExprVar
                                (line 24)
                                (id   X)))
                              (rhs (
                                ExprLit (
                                  lit (
                                    LitInteger
                                    (line    24)
                                    (integer 2)))))))))))))
                    (ExprLit (
                      lit (
                        LitAtom
                        (line 25)
                        (atom ok))))))))))))
          FormEof)))) |}]
