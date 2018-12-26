open Test_util

let%expect_test "test_fun_ref.beam" =
  print_ast "test_fun_ref.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun_ref.erl 1)
          (AttrMod 1 test_fun_ref)
          (AttrExport 3 (
            (f 0)
            (g 0)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody ((ExprLocalFunRef 6 f 0))))))
          (DeclFun
           9
           g
           0
           ((
             ClsFun 9
             ()
             ()
             (ExprBody (
               (ExprMatch 10 (PatVar 10 M) (ExprLit (LitAtom 10 test_fun_ref)))
               (ExprMatch 11 (PatVar 11 F) (ExprLit (LitAtom 11 f)))
               (ExprMatch 12 (PatVar 12 A) (ExprLit (LitInteger 12 0)))
               (ExprMatch 14
                 (PatUniversal 14)
                 (ExprRemoteFunRef 14
                   (AtomVarAtom       14 test_fun_ref)
                   (AtomVarAtom       14 f)
                   (IntegerVarInteger 14 0)))
               (ExprRemoteFunRef 15
                 (AtomVarVar    15 M)
                 (AtomVarVar    15 F)
                 (IntegerVarVar 15 A)))))))
          FormEof)))) |}]
