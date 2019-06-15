open Test_util

let%expect_test "test_string.beam" =
  print_ast "test_string.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_string.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_string))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 0)
              (h 0))))
          (DeclFun
            (line          6)
            (function_name f)
            (arity         0)
            (clauses ((
              ClsFun
              (line 6)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (lit (LitString (line 6) (str (Asciis abc%/_hoge)))))))))))))
          (DeclFun
            (line          9)
            (function_name g)
            (arity         0)
            (clauses ((
              ClsFun
              (line 9)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (
                      lit (
                        LitString
                        (line 9)
                        (str (
                          CharList (
                            12356
                            12429
                            12399
                            12395
                            12411
                            12408
                            12392
                            12385
                            12427
                            12396
                            12427
                            12434))))))))))))))
          (DeclFun
            (line          12)
            (function_name h)
            (arity         0)
            (clauses ((
              ClsFun
              (line 12)
              (patterns       ())
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((ExprLit (lit (LitString (line 12) (str (CharList ())))))))))))))
          FormEof))))
 |}]
