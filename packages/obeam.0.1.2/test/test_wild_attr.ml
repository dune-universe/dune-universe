open Test_util

let%expect_test "test_wild_attr.beam" =
  print_ast "test_wild_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_wild_attr.erl 1)
          (AttrMod 1 test_wild_attr)
          (AttrWild 4 compile (List ((Atom export_all))))
          (AttrWild 6 vsn (Atom 1.0.0))
          (AttrWild 8 on_load (
            Tuple 2 (
              (Atom    f)
              (Integer 0))))
          (AttrWild 10 behaviour (Atom   gen_server))
          (AttrWild 12 foo       (Binary bar))
          (DeclFun
           14
           f
           0
           ((
             ClsFun 14
             ()
             ()
             (ExprBody ((ExprLit (LitAtom 14 ok)))))))
          FormEof)))) |}]
