open Test_util

let%expect_test "test_fun_call.beam" =
  print_ast "test_fun_call.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun_call.erl 1)
          (AttrMod 1 test_fun_call)
          (AttrExport 3 (
            (f 0)
            (f 1)
            (f 2)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody (
               (ExprMatch 7 (PatVar 7 F) (ExprLit (LitAtom 7 f)))
               (ExprLocalCall 9
                 (ExprLit (LitAtom 9 f))
                 ((ExprLit (LitInteger 9 1))))
               (ExprLocalCall 10 (ExprVar 10 F) ((ExprLit (LitInteger 10 1))))
               (ExprLocalCall 11
                 (ExprCase 11
                   (ExprLit (LitAtom 11 true))
                   ((ClsCase 11 (PatUniversal 11) () (ExprBody ((ExprVar 11 F))))))
                 ((ExprLit (LitInteger 11 1)))))))))
          (DeclFun
           14
           f
           1
           ((
             ClsFun 14
             ((PatVar 14 N))
             ()
             (ExprBody (
               (ExprMatch 15 (PatVar 15 M) (ExprLit (LitAtom 15 test_fun_call)))
               (ExprMatch 16 (PatVar 16 F) (ExprLit (LitAtom 16 f)))
               (ExprRemoteCall 18 18
                 (ExprLit (LitAtom 18 test_fun_call))
                 (ExprLit (LitAtom 18 f))
                 ((ExprVar 18 N) (ExprLit (LitInteger 18 2))))
               (ExprRemoteCall 19 19
                 (ExprVar 19 M)
                 (ExprVar 19 F)
                 ((ExprVar 19 N) (ExprLit (LitInteger 19 2))))
               (ExprRemoteCall 20 20
                 (ExprCase 20
                   (ExprLit (LitAtom 20 true))
                   ((ClsCase 20 (PatUniversal 20) () (ExprBody ((ExprVar 20 M))))))
                 (ExprCase 20
                   (ExprLit (LitAtom 20 true))
                   ((ClsCase 20 (PatUniversal 20) () (ExprBody ((ExprVar 20 F))))))
                 ((ExprVar 20 N) (ExprLit (LitInteger 20 2)))))))))
          (DeclFun
           22
           f
           2
           ((
             ClsFun 22
             ((PatVar 22 N)
              (PatVar 22 M))
             ()
             (ExprBody ((
               ExprBinOp 22 +
               (ExprVar 22 N)
               (ExprVar 22 M)))))))
          FormEof)))) |}]
