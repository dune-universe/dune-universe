open Test_util

let%expect_test "test_map.beam" =
  print_ast "test_map.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_map.erl 1)
          (AttrMod 1 test_map)
          (AttrExport 3 (
            (f 0)
            (g 1)
            (h 1)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody ((
               ExprMapUpdate 6
               (ExprMapCreation 6 (
                 (ExprAssoc 6
                   (ExprLit (LitAtom    6 a))
                   (ExprLit (LitInteger 6 1)))
                 (ExprAssoc 6
                   (ExprLit (LitAtom    6 b))
                   (ExprLit (LitInteger 6 2)))))
               ((ExprAssocExact 6
                  (ExprLit (LitAtom    6 a))
                  (ExprLit (LitInteger 6 42)))
                (ExprAssoc 6
                  (ExprLit (LitAtom    6 c))
                  (ExprLit (LitInteger 6 3))))))))))
          (DeclFun
           9
           g
           1
           ((
             ClsFun 9
             ((PatMap 9 ((PatAssocExact 9 (PatLit (LitAtom 9 a)) (PatVar 9 N)))))
             ()
             (ExprBody ((ExprVar 9 N))))))
          (DeclFun
           12
           h
           1
           ((
             ClsFun 12
             ((PatVar 12 M))
             ((
               GuardSeq ((
                 Guard ((
                   GuardTestBinOp 12 andalso
                   (GuardTestBinOp 12 =:=
                     (GuardTestVar 12 M)
                     (GuardTestMapCreation 12 ((
                       GuardTestAssoc 12
                       (GuardTestLit (LitAtom    12 a))
                       (GuardTestLit (LitInteger 12 42))))))
                   (GuardTestBinOp 12 =:=
                     (GuardTestMapUpdate 12
                       (GuardTestVar 12 M)
                       ((
                         GuardTestAssocExact 12
                         (GuardTestLit (LitAtom    12 a))
                         (GuardTestLit (LitInteger 12 0)))))
                     (GuardTestMapCreation 12 ((
                       GuardTestAssoc 12
                       (GuardTestLit (LitAtom    12 a))
                       (GuardTestLit (LitInteger 12 0))))))))))))
             (ExprBody ((ExprVar 12 M))))))
          FormEof)))) |}]
