open Test_util

let%expect_test "test_tuple.beam" =
  print_ast "test_tuple.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_tuple.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_tuple))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 1)
              (h 1))))
          (AttrExportType
            (line 5)
            (type_arity_list (
              (t 0)
              (s 0))))
          (DeclType
            (line 8)
            (name t)
            (tvars ())
            (ty (TyAnyTuple (line 8))))
          (DeclType
            (line 11)
            (name s)
            (tvars ())
            (ty (
              TyTuple
              (line 11)
              (elements (
                (TyPredef (line 11) (name atom)    (args ()))
                (TyPredef (line 11) (name integer) (args ())))))))
          (DeclFun
            (line          14)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 14)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTuple
                    (line 14)
                    (elements (
                      (ExprLit (
                        lit (
                          LitAtom
                          (line 14)
                          (atom ok))))
                      (ExprLit (
                        lit (
                          LitInteger
                          (line    14)
                          (integer 42)))))))))))))))
          (DeclFun
            (line          17)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 17)
              (patterns ((
                PatTuple
                (line 17)
                (pats (
                  (PatLit (
                    lit (
                      LitAtom
                      (line 17)
                      (atom ok))))
                  (PatLit (
                    lit (
                      LitInteger
                      (line    17)
                      (integer 42)))))))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitAtom
                        (line 17)
                        (atom ok))))))))))))
          (DeclFun
            (line          20)
            (function_name h)
            (arity         1)
            (clauses ((
              ClsFun
              (line 20)
              (patterns ((
                PatVar
                (line 20)
                (id   T))))
              (guard_sequence ((
                GuardSeq (
                  guards ((
                    Guard (
                      guard_tests ((
                        GuardTestBinOp
                        (line 20)
                        (op   =:=)
                        (lhs (
                          GuardTestVar
                          (line 20)
                          (id   T)))
                        (rhs (
                          GuardTestTuple
                          (line 20)
                          (elements (
                            (GuardTestLit (
                              lit (
                                LitAtom
                                (line 20)
                                (atom ok))))
                            (GuardTestLit (
                              lit (
                                LitInteger
                                (line    20)
                                (integer 42)))))))))))))))))
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitAtom
                        (line 20)
                        (atom ok))))))))))))
          FormEof)))) |}]
