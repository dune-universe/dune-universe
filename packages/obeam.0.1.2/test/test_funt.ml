open Test_util

let%expect_test "test_funt.beam" =
  print_ast "test_funt.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_funt.erl 1)
          (AttrMod 1 test_funt)
          (AttrExport 3 ((g 0)))
          (DeclFun
           5
           f
           0
           ((
             ClsFun 5
             ()
             ()
             (ExprBody ((ExprLit (LitAtom 5 ok)))))))
          (DeclFun
           8
           g
           0
           ((
             ClsFun 8
             ()
             ()
             (ExprBody ((ExprLocalFunRef 8 f 0))))))
          FormEof)))) |}]
