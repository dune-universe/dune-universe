open Test_util

let%expect_test "test_fun.beam" =
  print_ast "test_fun.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_fun.erl 1)
          (AttrMod 1 test_fun)
          (AttrExport 3 (
            (g 0)
            (h 1)))
          (DeclFun
           6
           f
           0
           ((
             ClsFun 6
             ()
             ()
             (ExprBody ((ExprLit (LitInteger 7 0)))))))
          (DeclFun
           9
           g
           0
           ((
             ClsFun 9
             ()
             ()
             (ExprBody ((ExprLit (LitInteger 10 10)))))))
          (SpecFun 12
            ()
            h
            1
            ((TyFun 12 12 ((TyPredef 12 integer ())) (TyPredef 12 string ()))))
          (DeclFun
           13
           h
           1
           ((
             ClsFun 13
             ((PatUniversal 13))
             ()
             (ExprBody ((ExprLit (LitString 14 abcdefg)))))))
          (SpecFun 16
            ()
            i
            2
            ((
              TyFun 16 16
              ((TyPredef 16 integer ())
               (TyPredef 16 float   ()))
              (TyPredef 16 string ()))))
          (DeclFun
           17
           i
           2
           ((
             ClsFun 17
             ((PatUniversal 17)
              (PatUniversal 17))
             ()
             (ExprBody ((ExprLit (LitString 18 abracadabra)))))))
          FormEof)))) |}]
