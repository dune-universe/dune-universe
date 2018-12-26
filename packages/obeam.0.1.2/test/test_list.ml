open Test_util

let%expect_test "test_list.beam" =
  print_ast "test_list.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_list.erl 1)
          (AttrMod 1 test_list)
          (AttrExport 2 (
            (f 0)
            (g 1)))
          (DeclFun
           5
           f
           0
           ((
             ClsFun 5
             ()
             ()
             (ExprBody (
               (ExprMatch 6
                 (PatVar 6 List)
                 (ExprCons 6
                   (ExprLit (LitInteger 6 3))
                   (ExprCons 6
                     (ExprLit (LitInteger 6 1))
                     (ExprCons 6 (ExprLit (LitInteger 6 4)) (ExprNil 6)))))
               (ExprMatch 7
                 (PatVar 7 List)
                 (ExprCons 7
                   (ExprLit (LitInteger 7 3))
                   (ExprCons 7
                     (ExprLit (LitInteger 7 1))
                     (ExprCons 7 (ExprLit (LitInteger 7 4)) (ExprNil 7)))))
               (ExprMatch 9
                 (PatVar 9 List)
                 (ExprCons 9
                   (ExprLit (LitInteger 9 3))
                   (ExprCons 9
                     (ExprLit (LitInteger 9 1))
                     (ExprCons 9 (ExprLit (LitInteger 9 4)) (ExprNil 9)))))
               (ExprMatch 10
                 (PatVar 10 List)
                 (ExprCons 10
                   (ExprLit (LitInteger 10 3))
                   (ExprCons 10
                     (ExprLit (LitInteger 10 1))
                     (ExprCons 10 (ExprLit (LitInteger 10 4)) (ExprNil 10)))))
               (ExprMatch 12
                 (PatVar 12 ImproperList)
                 (ExprCons 12
                   (ExprLit (LitInteger 12 3))
                   (ExprCons 12
                     (ExprLit (LitInteger 12 1))
                     (ExprLit (LitInteger 12 4)))))
               (ExprListComprehension 14
                 (ExprBinOp 14 * (ExprVar 14 X) (ExprLit (LitInteger 14 2)))
                 ((QualifierGenerator 14
                    (PatVar  14 X)
                    (ExprVar 14 List))
                  (QualifierFilter (
                    ExprBinOp 14 >= (ExprVar 14 X) (ExprLit (LitInteger 14 3)))))))))))
          (DeclFun
           17
           g
           1
           ((
             ClsFun 17
             ((PatVar 17 List))
             ()
             (ExprBody (
               (ExprMatch 18
                 (PatCons 18
                   (PatLit (LitInteger 18 3))
                   (PatCons 18
                     (PatLit (LitInteger 18 1))
                     (PatCons 18 (PatLit (LitInteger 18 4)) (PatNil 18))))
                 (ExprVar 18 List))
               (ExprMatch 19
                 (PatCons 19
                   (PatLit (LitInteger 19 3))
                   (PatCons 19
                     (PatLit (LitInteger 19 1))
                     (PatCons 19 (PatLit (LitInteger 19 4)) (PatNil 19))))
                 (ExprVar 19 List))
               (ExprMatch 21
                 (PatCons 21
                   (PatLit (LitInteger 21 3))
                   (PatCons 21
                     (PatLit (LitInteger 21 1))
                     (PatCons 21 (PatLit (LitInteger 21 4)) (PatNil 21))))
                 (ExprVar 21 List))
               (ExprMatch 22
                 (PatCons 22
                   (PatLit (LitInteger 22 3))
                   (PatCons 22
                     (PatLit (LitInteger 22 1))
                     (PatCons 22 (PatLit (LitInteger 22 4)) (PatNil 22))))
                 (ExprVar 22 List))
               (ExprMatch 24
                 (PatCons 24
                   (PatLit (LitInteger 24 3))
                   (PatCons 24
                     (PatLit (LitInteger 24 1))
                     (PatLit (LitInteger 24 4))))
                 (ExprVar 24 List)))))))
          FormEof)))) |}]
