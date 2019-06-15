open Test_util

let%expect_test "test_fun.beam" =
  print_ast "test_fun.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_fun.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_fun))
          (AttrExport
            (line 3)
            (function_arity_list (
              (g 0)
              (h 1))))
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
                        LitInteger
                        (line    7)
                        (integer 0))))))))))))
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
                        LitInteger
                        (line    10)
                        (integer 10))))))))))))
          (SpecFun
            (line 12)
            (module_name ())
            (function_name h)
            (arity         1)
            (specs ((
              TyFun
              (line        12)
              (line_params 12)
              (params ((
                TyPredef
                (line 12)
                (name integer)
                (args ()))))
              (ret (
                TyPredef
                (line 12)
                (name string)
                (args ())))))))
          (DeclFun
            (line          13)
            (function_name h)
            (arity         1)
            (clauses ((
              ClsFun
              (line 13)
              (patterns ((PatUniversal (line 13))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (lit (LitString (line 14) (str (Asciis abcdefg)))))))))))))
          (SpecFun
            (line 16)
            (module_name ())
            (function_name i)
            (arity         2)
            (specs ((
              TyFun
              (line        16)
              (line_params 16)
              (params (
                (TyPredef (line 16) (name integer) (args ()))
                (TyPredef (line 16) (name float)   (args ()))))
              (ret (
                TyPredef
                (line 16)
                (name string)
                (args ())))))))
          (DeclFun
            (line          17)
            (function_name i)
            (arity         2)
            (clauses ((
              ClsFun
              (line 17)
              (patterns (
                (PatUniversal (line 17))
                (PatUniversal (line 17))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprLit (lit (LitString (line 18) (str (Asciis abracadabra)))))))))))))
          FormEof)))) |}]
