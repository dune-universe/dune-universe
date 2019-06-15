open Test_util

let%expect_test "test_compound_pattern.beam" =
  print_ast "test_compound_pattern.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_compound_pattern.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_compound_pattern))
          (AttrExport (line 3) (function_arity_list ((f 1))))
          (DeclFun
            (line          5)
            (function_name f)
            (arity         1)
            (clauses ((
              ClsFun
              (line 5)
              (patterns ((
                PatCompound
                (line 5)
                (lhs (
                  PatTuple
                  (line 5)
                  (pats (
                    (PatVar
                      (line 5)
                      (id   A))
                    (PatLit (
                      lit (
                        LitAtom
                        (line 5)
                        (atom b))))))))
                (rhs (
                  PatTuple
                  (line 5)
                  (pats (
                    (PatLit (
                      lit (
                        LitAtom
                        (line 5)
                        (atom a))))
                    (PatVar
                      (line 5)
                      (id   B)))))))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprTuple
                    (line 5)
                    (elements (
                      (ExprVar (line 5) (id A))
                      (ExprVar (line 5) (id B)))))))))))))
          FormEof)))) |}]
