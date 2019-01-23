open Test_util

let%expect_test "test_char.beam" =
  print_ast "test_char.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_char.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_char))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 0)
              (g 0))))
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
                    ExprLit (
                      lit (
                        LitChar
                        (line  6)
                        (uchar U+0061))))))))))))
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
                        LitChar
                        (line  9)
                        (uchar U+3042))))))))))))
          FormEof)))) |}]
