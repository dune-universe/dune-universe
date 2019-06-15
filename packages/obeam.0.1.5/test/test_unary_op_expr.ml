open Test_util

let%expect_test "test_unary_op_expr.beam" =
  print_ast "test_unary_op_expr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_unary_op_expr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_unary_op_expr))
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
                  exprs (
                    (ExprMatch
                      (line 7)
                      (pattern (PatUniversal (line 7)))
                      (body (
                        ExprUnaryOp
                        (line 7)
                        (op   not)
                        (operand (
                          ExprLit (
                            lit (
                              LitAtom
                              (line 7)
                              (atom true))))))))
                    (ExprUnaryOp
                      (line 8)
                      (op   -)
                      (operand (
                        ExprLit (
                          lit (
                            LitInteger
                            (line    8)
                            (integer 1))))))))))))))
          FormEof)))) |}]
