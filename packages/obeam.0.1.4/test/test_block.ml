open Test_util

let%expect_test "test_block.beam" =
  print_ast "test_block.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_block.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_block))
          (AttrExport (line 3) (function_arity_list ((f 0))))
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
                    ExprBlock
                    (line 7)
                    (exprs (
                      (ExprMatch
                        (line 8)
                        (pattern (
                          PatVar
                          (line 8)
                          (id   N)))
                        (body (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    8)
                              (integer 1))))))
                      (ExprMatch
                        (line 9)
                        (pattern (
                          PatVar
                          (line 9)
                          (id   M)))
                        (body (
                          ExprLit (
                            lit (
                              LitInteger
                              (line    9)
                              (integer 2))))))
                      (ExprBinOp
                        (line 10)
                        (op   +)
                        (lhs (ExprVar (line 10) (id N)))
                        (rhs (ExprVar (line 10) (id M)))))))))))))))
          FormEof)))) |}]
