open Test_util

let%expect_test "test_fun_expr.beam" =
  print_ast "test_fun_expr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun_expr.erl 1)
          (AttrMod 1 test_fun_expr)
          (AttrExport 3 (
            (f 0)
            (g 0)
            (h 0)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody ((
               ExprFun 7
               ()
               ((ClsFun 7
                  ((PatLit (LitInteger 7 42)))
                  ()
                  (ExprBody ((ExprLit (LitInteger 7 42)))))
                (ClsFun 8
                  ((PatUniversal 8))
                  ()
                  (ExprBody ((ExprLit (LitInteger 8 57))))))))))))
          (DeclFun
           12
           g
           0
           ((
             ClsFun 12
             ()
             ()
             (ExprBody ((
               ExprFun 13
               (F)
               ((ClsFun 13
                  ((PatLit (LitInteger 13 42)))
                  ()
                  (ExprBody ((
                    ExprLocalCall 13
                    (ExprVar 13 F)
                    ((ExprLit (LitInteger 13 57)))))))
                (ClsFun 14
                  ((PatUniversal 14))
                  ()
                  (ExprBody ((ExprLit (LitInteger 14 57))))))))))))
          (DeclFun
           18
           h
           0
           ((
             ClsFun 18
             ()
             ()
             (ExprBody ((
               ExprFun 19
               ()
               ((ClsFun 19
                  ((PatVar 19 N))
                  ((
                    GuardSeq ((
                      Guard ((
                        GuardTestBinOp 19 =:=
                        (GuardTestVar 19 N)
                        (GuardTestLit (LitInteger 19 42))))))))
                  (ExprBody ((ExprVar 19 N))))
                (ClsFun 20
                  ((PatUniversal 20))
                  ()
                  (ExprBody ((ExprLit (LitInteger 20 57))))))))))))
          FormEof)))) |}]
