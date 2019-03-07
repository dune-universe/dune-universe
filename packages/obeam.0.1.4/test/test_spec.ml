open Test_util

let%expect_test "test_spec.beam" =
  print_ast "test_spec.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_spec.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_spec))
          (AttrExport
            (line 3)
            (function_arity_list (
              (f 1)
              (g 1))))
          (Callback
            (line          6)
            (function_name foo)
            (arity         1)
            (specs ((
              TyFun
              (line        6)
              (line_params 6)
              (params ((
                TyLit (
                  lit (
                    LitAtom
                    (line 6)
                    (atom ok))))))
              (ret (
                TyPredef
                (line 6)
                (name term)
                (args ())))))))
          (SpecFun
            (line 9)
            (module_name ())
            (function_name f)
            (arity         1)
            (specs ((
              TyContFun
              (line 9)
              (function_type (
                TyFun
                (line        9)
                (line_params 9)
                (params ((
                  TyVar
                  (line 9)
                  (id   A))))
                (ret (
                  TyPredef
                  (line 9)
                  (name integer)
                  (args ())))))
              (constraints (
                TyCont (
                  constraints ((
                    TyContRel
                    (line 9)
                    (constraint_kind (TyContIsSubType (line 9)))
                    (lhs (
                      TyVar
                      (line 9)
                      (id   A)))
                    (rhs (
                      TyPredef
                      (line 9)
                      (name integer)
                      (args ()))))))))))))
          (DeclFun
            (line          10)
            (function_name f)
            (arity         1)
            (clauses ((
              ClsFun
              (line 10)
              (patterns ((
                PatVar
                (line 10)
                (id   N))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprVar
                    (line 10)
                    (id   N))))))))))
          (SpecFun
            (line 13)
            (module_name ())
            (function_name g)
            (arity         1)
            (specs ((
              TyContFun
              (line 13)
              (function_type (
                TyFun
                (line        13)
                (line_params 13)
                (params ((
                  TyVar
                  (line 13)
                  (id   A))))
                (ret (
                  TyVar
                  (line 13)
                  (id   B)))))
              (constraints (
                TyCont (
                  constraints (
                    (TyContRel
                      (line 13)
                      (constraint_kind (TyContIsSubType (line 13)))
                      (lhs (
                        TyVar
                        (line 13)
                        (id   A)))
                      (rhs (
                        TyPredef
                        (line 13)
                        (name integer)
                        (args ()))))
                    (TyContRel
                      (line 14)
                      (constraint_kind (TyContIsSubType (line 14)))
                      (lhs (
                        TyVar
                        (line 14)
                        (id   B)))
                      (rhs (
                        TyPredef
                        (line 14)
                        (name integer)
                        (args ()))))))))))))
          (DeclFun
            (line          15)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 15)
              (patterns ((
                PatVar
                (line 15)
                (id   N))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprBinOp
                    (line 16)
                    (op   *)
                    (lhs (ExprVar (line 16) (id N)))
                    (rhs (ExprVar (line 16) (id N))))))))))))
          (SpecFun
            (line 19)
            (module_name (lists))
            (function_name member)
            (arity         2)
            (specs ((
              TyFun
              (line        19)
              (line_params 19)
              (params (
                (TyPredef
                  (line 19)
                  (name number)
                  (args ()))
                (TyPredef
                  (line 19)
                  (name list)
                  (args ((
                    TyPredef
                    (line 19)
                    (name number)
                    (args ())))))))
              (ret (
                TyLit (
                  lit (
                    LitAtom
                    (line 19)
                    (atom boolean)))))))))
          FormEof)))) |}]
