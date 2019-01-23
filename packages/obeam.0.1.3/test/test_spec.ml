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
          (SpecFun
            (line 6)
            (module_name ())
            (function_name f)
            (arity         1)
            (specs ((
              TyContFun
              (line 6)
              (function_type (
                TyFun
                (line        6)
                (line_params 6)
                (params ((
                  TyVar
                  (line 6)
                  (id   A))))
                (ret (
                  TyPredef
                  (line 6)
                  (name integer)
                  (args ())))))
              (constraints (
                TyCont (
                  constraints ((
                    TyContRel
                    (line 6)
                    (constraint_kind (TyContIsSubType (line 6)))
                    (lhs (
                      TyVar
                      (line 6)
                      (id   A)))
                    (rhs (
                      TyPredef
                      (line 6)
                      (name integer)
                      (args ()))))))))))))
          (DeclFun
            (line          7)
            (function_name f)
            (arity         1)
            (clauses ((
              ClsFun
              (line 7)
              (patterns ((
                PatVar
                (line 7)
                (id   N))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprVar
                    (line 7)
                    (id   N))))))))))
          (SpecFun
            (line 10)
            (module_name ())
            (function_name g)
            (arity         1)
            (specs ((
              TyContFun
              (line 10)
              (function_type (
                TyFun
                (line        10)
                (line_params 10)
                (params ((
                  TyVar
                  (line 10)
                  (id   A))))
                (ret (
                  TyVar
                  (line 10)
                  (id   B)))))
              (constraints (
                TyCont (
                  constraints (
                    (TyContRel
                      (line 10)
                      (constraint_kind (TyContIsSubType (line 10)))
                      (lhs (
                        TyVar
                        (line 10)
                        (id   A)))
                      (rhs (
                        TyPredef
                        (line 10)
                        (name integer)
                        (args ()))))
                    (TyContRel
                      (line 11)
                      (constraint_kind (TyContIsSubType (line 11)))
                      (lhs (
                        TyVar
                        (line 11)
                        (id   B)))
                      (rhs (
                        TyPredef
                        (line 11)
                        (name integer)
                        (args ()))))))))))))
          (DeclFun
            (line          12)
            (function_name g)
            (arity         1)
            (clauses ((
              ClsFun
              (line 12)
              (patterns ((
                PatVar
                (line 12)
                (id   N))))
              (guard_sequence ())
              (body (
                ExprBody (
                  exprs ((
                    ExprBinOp
                    (line 13)
                    (op   *)
                    (lhs (ExprVar (line 13) (id N)))
                    (rhs (ExprVar (line 13) (id N))))))))))))
          FormEof)))) |}]
