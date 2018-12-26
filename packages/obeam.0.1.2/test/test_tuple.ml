open Test_util

let%expect_test "test_tuple.beam" =
  print_ast "test_tuple.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_tuple.erl 1)
          (AttrMod 1 test_tuple)
          (AttrExport 3 (
            (f 0)
            (g 1)
            (h 1)))
          (AttrExportType 5 (
            (t 0)
            (s 0)))
          (DeclType 8 t () (TyAnyTuple 8))
          (DeclType 11 s
            ()
            (TyTuple 11 (
              (TyPredef 11 atom    ())
              (TyPredef 11 integer ()))))
          (DeclFun
           14
           f
           0
           ((
             ClsFun 14
             ()
             ()
             (ExprBody ((
               ExprTuple 14 (
                 (ExprLit (LitAtom    14 ok))
                 (ExprLit (LitInteger 14 42)))))))))
          (DeclFun
           17
           g
           1
           ((
             ClsFun 17
             ((
               PatTuple 17 (
                 (PatLit (LitAtom    17 ok))
                 (PatLit (LitInteger 17 42)))))
             ()
             (ExprBody ((ExprLit (LitAtom 17 ok)))))))
          (DeclFun
           20
           h
           1
           ((
             ClsFun 20
             ((PatVar 20 T))
             ((
               GuardSeq ((
                 Guard ((
                   GuardTestBinOp 20 =:=
                   (GuardTestVar 20 T)
                   (GuardTestTuple 20 (
                     (GuardTestLit (LitAtom    20 ok))
                     (GuardTestLit (LitInteger 20 42))))))))))
             (ExprBody ((ExprLit (LitAtom 20 ok)))))))
          FormEof)))) |}]
