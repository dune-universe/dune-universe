open Test_util

let%expect_test "test_wild_attr.beam" =
  print_ast "test_wild_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_wild_attr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_wild_attr))
          (AttrWild
            (line      4)
            (attribute compile)
            (term (List ((Atom export_all)))))
          (AttrWild
            (line      6)
            (attribute vsn)
            (term (Atom 1.0.0)))
          (AttrWild
            (line      8)
            (attribute on_load)
            (term (
              Tuple 2 (
                (Atom    f)
                (Integer 0)))))
          (AttrWild
            (line      10)
            (attribute behaviour)
            (term (Atom gen_server)))
          (AttrWild
            (line      12)
            (attribute foo)
            (term (Binary bar)))
          (DeclFun
            (line          14)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 14)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitAtom
                        (line 14)
                        (atom ok))))))))))))
          FormEof)))) |}]
