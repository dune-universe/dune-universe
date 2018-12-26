open Test_util

let%expect_test "test_case.beam" =
  print_ast "test_case.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_case.erl 1)
          (AttrMod 1 test_case)
          (AttrExport 3 ((f 1)))
          (DeclFun
           6
           f
           1
           ((
             ClsFun 6
             ((PatVar 6 R))
             ()
             (ExprBody ((
               ExprCase 7
               (ExprVar 7 R)
               ((ClsCase 8
                  (PatLit (LitAtom 8 ok))
                  ()
                  (ExprBody ((ExprLit (LitInteger 8 1)))))
                (ClsCase 9
                  (PatLit (LitAtom 9 error))
                  ()
                  (ExprBody ((ExprLit (LitInteger 9 2))))))))))))
          FormEof)))) |}]
