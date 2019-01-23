open Test_util

let%expect_test "test_fun_ref.beam" =
  print_ast "test_fun_ref.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_fun_ref.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_fun_ref))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 0))))
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
                    ExprLocalFunRef
                    (line          6)
                    (function_name f)
                    (arity         0))))))))))
          (DeclFun
            (line          9)
            (function_name g)
            (arity         0)
            (clauses ((
              ClsFun
              (line 9)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs (
                    (ExprMatch
                      (line 10)
                      (pattern (
                        PatVar
                        (line 10)
                        (id   M)))
                      (body (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 10)
                            (atom test_fun_ref))))))
                    (ExprMatch
                      (line 11)
                      (pattern (
                        PatVar
                        (line 11)
                        (id   F)))
                      (body (
                        ExprLit (
                          lit (
                            LitAtom
                            (line 11)
                            (atom f))))))
                    (ExprMatch
                      (line 12)
                      (pattern (
                        PatVar
                        (line 12)
                        (id   A)))
                      (body (
                        ExprLit (
                          lit (
                            LitInteger
                            (line    12)
                            (integer 0))))))
                    (ExprMatch
                      (line 14)
                      (pattern (PatUniversal (line 14)))
                      (body (
                        ExprRemoteFunRef
                        (line 14)
                        (module_name   (AtomVarAtom (line 14) (atom test_fun_ref)))
                        (function_name (AtomVarAtom (line 14) (atom f)))
                        (arity (
                          IntegerVarInteger
                          (line    14)
                          (integer 0))))))
                    (ExprRemoteFunRef
                      (line 15)
                      (module_name   (AtomVarVar    (line 15) (id M)))
                      (function_name (AtomVarVar    (line 15) (id F)))
                      (arity         (IntegerVarVar (line 15) (id A))))))))))))
          FormEof)))) |}]
