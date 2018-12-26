open Test_util

let%expect_test "test_guard.beam" =
  print_ast "test_guard.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_guard.erl 1)
          (AttrMod 1 test_guard)
          (AttrExport 3 ((f 1)))
          (DeclFun
           6
           f
           1
           ((ClsFun 6
              ((PatVar 6 N))
              ((
                GuardSeq ((
                  Guard ((
                    GuardTestCall 6 (LitAtom 6 is_integer) ((GuardTestVar 6 N))))))))
              (ExprBody ((ExprLit (LitInteger 7 0)))))
            (ClsFun 8
              ((PatUniversal 8))
              ()
              (ExprBody ((ExprLit (LitInteger 9 2)))))))
          FormEof)))) |}]
