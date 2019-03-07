open Test_util

let%expect_test "test_guard.beam" =
  print_ast "test_guard.beam";
  [%expect {|
(Ok (
  AbstractCode (
    ModDecl (
      (AttrFile
        (line      1)
        (file      test_guard.erl)
        (file_line 1))
      (AttrMod
        (line        1)
        (module_name test_guard))
      (AttrExport (line 3) (function_arity_list ((f 1))))
      (DeclFun
        (line          6)
        (function_name f)
        (arity         1)
        (clauses (
          (ClsFun
            (line 6)
            (patterns ((
              PatVar
              (line 6)
              (id   N))))
            (guard_sequence ((
              GuardSeq (
                guards ((
                  Guard (
                    guard_tests ((
                      GuardTestCall
                      (line 6)
                      (function_name (
                        LitAtom
                        (line 6)
                        (atom is_integer)))
                      (args ((
                        GuardTestVar
                        (line 6)
                        (id   N)))))))))))))
            (body (
              ExprBody (
                exprs ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    7)
                      (integer 0)))))))))
          (ClsFun
            (line 8)
            (patterns ((
              PatVar
              (line 8)
              (id   N))))
            (guard_sequence ((
              GuardSeq (
                guards ((
                  Guard (
                    guard_tests ((
                      GuardTestRemoteCall
                      (line               8)
                      (line_remote        8)
                      (line_module_name   8)
                      (module_name        erlang)
                      (line_function_name 8)
                      (function_name      is_integer)
                      (args ((
                        GuardTestVar
                        (line 8)
                        (id   N)))))))))))))
            (body (
              ExprBody (
                exprs ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    9)
                      (integer 1)))))))))
          (ClsFun
            (line 10)
            (patterns ((
              PatVar
              (line 10)
              (id   N))))
            (guard_sequence ((
              GuardSeq (
                guards ((
                  Guard (
                    guard_tests ((
                      GuardTestRemoteCall
                      (line               10)
                      (line_remote        10)
                      (line_module_name   10)
                      (module_name        erlang)
                      (line_function_name 10)
                      (function_name      =:=)
                      (args (
                        (GuardTestVar
                          (line 10)
                          (id   N))
                        (GuardTestLit (
                          lit (
                            LitInteger
                            (line    10)
                            (integer 2)))))))))))))))
            (body (
              ExprBody (
                exprs ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    11)
                      (integer 2)))))))))
          (ClsFun
            (line 12)
            (patterns ((
              PatVar
              (line 12)
              (id   N))))
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
                        (id   N)))
                      (rhs (
                        GuardTestUnaryOp
                        (line 12)
                        (op   -)
                        (operand (
                          GuardTestLit (
                            lit (
                              LitInteger
                              (line    12)
                              (integer 1))))))))))))))))
            (body (
              ExprBody (
                exprs ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    13)
                      (integer 3)))))))))
          (ClsFun
            (line 14)
            (patterns ((PatUniversal (line 14))))
            (guard_sequence ())
            (body (
              ExprBody (
                exprs ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    15)
                      (integer 4))))))))))))
      FormEof)))) |}]
