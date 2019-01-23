open Test_util

let%expect_test "test_map.beam" =
  print_ast "test_map.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_map.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_map))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 1)
              (h 1))))
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
                    ExprMapUpdate
                    (line 6)
                    (map (
                      ExprMapCreation
                      (line 6)
                      (assocs (
                        (ExprAssoc
                          (line 6)
                          (key (
                            ExprLit (
                              lit (
                                LitAtom
                                (line 6)
                                (atom a)))))
                          (value (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    6)
                                (integer 1))))))
                        (ExprAssoc
                          (line 6)
                          (key (
                            ExprLit (
                              lit (
                                LitAtom
                                (line 6)
                                (atom b)))))
                          (value (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    6)
                                (integer 2))))))))))
                    (assocs (
                      (ExprAssocExact
                        (line 6)
                        (key (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 6)
                              (atom a)))))
                        (value (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    6)
                              (integer 42))))))
                      (ExprAssoc
                        (line 6)
                        (key (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 6)
                              (atom c)))))
                        (value (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    6)
                              (integer 3)))))))))))))))))
          (DeclFun
            (line          9)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 9)
              (patterns ((
                PatMap
                (line 9)
                (assocs ((
                  PatAssocExact
                  (line 9)
                  (key (
                    PatLit (
                      lit (
                        LitAtom
                        (line 9)
                        (atom a)))))
                  (value (
                    PatVar
                    (line 9)
                    (id   N)))))))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprVar
                    (line 9)
                    (id   N))))))))))
          (DeclFun
            (line          12)
            (function_name h)
            (arity         1)
            (clauses ((
              ClsFun
              (line 12)
              (patterns ((
                PatVar
                (line 12)
                (id   M))))
              (guard_sequence ((
                GuardSeq (
                  guards ((
                    Guard (
                      guard_tests ((
                        GuardTestBinOp
                        (line 12)
                        (op   andalso)
                        (lhs (
                          GuardTestBinOp
                          (line 12)
                          (op   =:=)
                          (lhs (
                            GuardTestVar
                            (line 12)
                            (id   M)))
                          (rhs (
                            GuardTestMapCreation
                            (line 12)
                            (assocs ((
                              GuardTestAssoc
                              (line 12)
                              (key (
                                GuardTestLit (
                                  lit (
                                    LitAtom
                                    (line 12)
                                    (atom a)))))
                              (value (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    12)
                                    (integer 42))))))))))))
                        (rhs (
                          GuardTestBinOp
                          (line 12)
                          (op   =:=)
                          (lhs (
                            GuardTestMapUpdate
                            (line 12)
                            (map (
                              GuardTestVar
                              (line 12)
                              (id   M)))
                            (assocs ((
                              GuardTestAssocExact
                              (line 12)
                              (key (
                                GuardTestLit (
                                  lit (
                                    LitAtom
                                    (line 12)
                                    (atom a)))))
                              (value (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    12)
                                    (integer 0))))))))))
                          (rhs (
                            GuardTestMapCreation
                            (line 12)
                            (assocs ((
                              GuardTestAssoc
                              (line 12)
                              (key (
                                GuardTestLit (
                                  lit (
                                    LitAtom
                                    (line 12)
                                    (atom a)))))
                              (value (
                                GuardTestLit (
                                  lit (
                                    LitInteger
                                    (line    12)
                                    (integer 0)))))))))))))))))))))
              (body (
                ExprBody (
                  exprs ((
                    ExprVar
                    (line 12)
                    (id   M))))))))))
          FormEof)))) |}]
