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
                (patterns ((PatUniversal (line 8))))
                (guard_sequence ())
                (body (
                  ExprBody (
                    exprs ((
                      ExprLit (
                        lit (
                          LitInteger
                          (line    9)
                          (integer 2))))))))))))
          FormEof)))) |}]
