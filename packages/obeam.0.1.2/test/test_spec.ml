open Test_util

let%expect_test "test_spec.beam" =
  print_ast "test_spec.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_spec.erl 1)
          (AttrMod 1 test_spec)
          (AttrExport 3 (
            (f 1)
            (g 1)))
          (SpecFun 6
            ()
            f
            1
            ((
              TyContFun 6
              (TyFun 6 6 ((TyVar 6 A)) (TyPredef 6 integer ()))
              (TyCont ((
                TyContRel 6
                (TyContIsSubType 6)
                (TyVar 6 A)
                (TyPredef 6 integer ())))))))
          (DeclFun
           7
           f
           1
           ((ClsFun 7 ((PatVar 7 N)) () (ExprBody ((ExprVar 7 N))))))
          (SpecFun 10
            ()
            g
            1
            ((
              TyContFun 10
              (TyFun 10 10 ((TyVar 10 A)) (TyVar 10 B))
              (TyCont (
                (TyContRel 10
                  (TyContIsSubType 10)
                  (TyVar 10 A)
                  (TyPredef 10 integer ()))
                (TyContRel 11
                  (TyContIsSubType 11)
                  (TyVar 11 B)
                  (TyPredef 11 integer ())))))))
          (DeclFun
           12
           g
           1
           ((
             ClsFun 12
             ((PatVar 12 N))
             ()
             (ExprBody ((
               ExprBinOp 13 *
               (ExprVar 13 N)
               (ExprVar 13 N)))))))
          FormEof)))) |}]
