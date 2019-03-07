open Test_util

let%expect_test "test_record.beam" =
  print_ast "test_record.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_record.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_record))
          (DeclRecord
            (line 4)
            (fields (
              (RecordField
                (line            4)
                (line_field_name 4)
                (field_name      a)
                (ty           ())
                (default_expr ()))
              (RecordField
                (line            5)
                (line_field_name 5)
                (field_name      b)
                (ty ())
                (default_expr ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    5)
                      (integer 42)))))))
              (RecordField
                (line            6)
                (line_field_name 6)
                (field_name      c)
                (ty ((
                  TyPredef
                  (line 6)
                  (name string)
                  (args ()))))
                (default_expr ()))
              (RecordField
                (line            7)
                (line_field_name 7)
                (field_name      d)
                (ty ((
                  TyPredef
                  (line 7)
                  (name integer)
                  (args ()))))
                (default_expr ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    7)
                      (integer 57))))))))))
          (AttrExport
            (line 9)
            (function_arity_list (
              (f 0)
              (g 1))))
          (DeclFun
            (line          11)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 11)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 12)
                      (pattern (
                        PatVar
                        (line 12)
                        (id   R)))
                      (body (
                        ExprRecord
                        (line 12)
                        (name r)
                        (record_fields (
                          (RecordFieldForExpr
                            (line      12)
                            (line_name 12)
                            (name      a)
                            (value (
                              ExprLit (
                                lit (
                                  LitInteger
                                  (line    12)
                                  (integer 3))))))
                          (RecordFieldForExpr
                            (line      12)
                            (line_name 12)
                            (name      c)
                            (value (
                              ExprLit (
                                lit (LitString (line 12) (str (Asciis hello))))))))))))
                    (ExprMatch
                      (line 13)
                      (pattern (PatUniversal (line 13)))
                      (body (
                        ExprRecordFieldAccess
                        (line 13)
                        (expr (
                          ExprVar
                          (line 13)
                          (id   R)))
                        (name            r)
                        (line_field_name 13)
                        (field_name      c))))
                    (ExprMatch
                      (line 14)
                      (pattern (
                        PatVar
                        (line 14)
                        (id   Index)))
                      (body (
                        ExprRecordFieldIndex
                        (line            14)
                        (name            r)
                        (line_field_name 14)
                        (field_name      b))))
                    (ExprRecordUpdate
                      (line 15)
                      (expr (
                        ExprVar
                        (line 15)
                        (id   R)))
                      (name r)
                      (update_fields (
                        (RecordFieldForExpr
                          (line      15)
                          (line_name 15)
                          (name      a)
                          (value (
                            ExprLit (
                              lit (
                                LitInteger
                                (line    15)
                                (integer 100))))))
                        (RecordFieldForExpr
                          (line      15)
                          (line_name 15)
                          (name      c)
                          (value (
                            ExprLit (
                              lit (LitString (line 15) (str (Asciis hoge))))))))))))))))))
          (DeclFun
            (line          17)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 17)
              (patterns ((
                PatVar
                (line 17)
                (id   R))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 18)
                      (pattern (PatUniversal (line 18)))
                      (body (
                        ExprCase
                        (line 19)
                        (expr (
                          ExprVar
                          (line 19)
                          (id   R)))
                        (clauses (
                          (ClsCase
                            (line 20)
                            (pattern (PatUniversal (line 20)))
                            (guard_sequence ((
                              GuardSeq (
                                guards ((
                                  Guard (
                                    guard_tests ((
                                      GuardTestRecord
                                      (line 20)
                                      (name r)
                                      (record_fields (
                                        (20
                                          (AtomWildcardAtom
                                            (line 20)
                                            (atom a))
                                          (GuardTestLit (
                                            lit (
                                              LitAtom
                                              (line 20)
                                              (atom true)))))
                                        (20
                                          (AtomWildcardWildcard (line 20))
                                          (GuardTestLit (
                                            lit (
                                              LitInteger
                                              (line    20)
                                              (integer 111))))))))))))))))
                            (body (
                              ExprBody (
                                exprs ((
                                  ExprLit (
                                    lit (
                                      LitAtom
                                      (line 20)
                                      (atom foo)))))))))
                          (ClsCase
                            (line 21)
                            (pattern (PatUniversal (line 21)))
                            (guard_sequence ((
                              GuardSeq (
                                guards ((
                                  Guard (
                                    guard_tests ((
                                      GuardTestRecordFieldAccess
                                      (line 21)
                                      (record (
                                        GuardTestVar
                                        (line 21)
                                        (id   R)))
                                      (name            r)
                                      (line_field_name 21)
                                      (field_name      b))))))))))
                            (body (
                              ExprBody (
                                exprs ((
                                  ExprLit (
                                    lit (
                                      LitAtom
                                      (line 21)
                                      (atom bar)))))))))
                          (ClsCase
                            (line 22)
                            (pattern (PatUniversal (line 22)))
                            (guard_sequence ((
                              GuardSeq (
                                guards ((
                                  Guard (
                                    guard_tests ((
                                      GuardTestRecordFieldIndex
                                      (line            22)
                                      (name            r)
                                      (line_field_name 22)
                                      (field_name      c))))))))))
                            (body (
                              ExprBody (
                                exprs ((
                                  ExprLit (
                                    lit (
                                      LitAtom
                                      (line 22)
                                      (atom baz))))))))))))))
                    (ExprMatch
                      (line 24)
                      (pattern (PatUniversal (line 24)))
                      (body (
                        ExprCase
                        (line 25)
                        (expr (
                          ExprVar
                          (line 25)
                          (id   R)))
                        (clauses (
                          (ClsCase
                            (line 26)
                            (pattern (
                              PatRecordFieldIndex
                              (line            26)
                              (name            r)
                              (line_field_name 26)
                              (field_name      a)))
                            (guard_sequence ())
                            (body (
                              ExprBody (
                                exprs ((
                                  ExprLit (
                                    lit (
                                      LitAtom
                                      (line 26)
                                      (atom foo)))))))))
                          (ClsCase
                            (line 27)
                            (pattern (
                              PatRecord
                              (line 27)
                              (name r)
                              (record_fields (
                                (27
                                  (AtomWildcardAtom
                                    (line 27)
                                    (atom a))
                                  (PatLit (
                                    lit (
                                      LitAtom
                                      (line 27)
                                      (atom true)))))
                                (27
                                  (AtomWildcardWildcard (line 27))
                                  (PatLit (
                                    lit (
                                      LitInteger
                                      (line    27)
                                      (integer 111)))))))))
                            (guard_sequence ())
                            (body (
                              ExprBody (
                                exprs ((
                                  ExprLit (
                                    lit (
                                      LitAtom
                                      (line 27)
                                      (atom bar))))))))))))))
                    (ExprLit (
                      lit (
                        LitAtom
                        (line 29)
                        (atom ok))))))))))))
          FormEof)))) |}]
