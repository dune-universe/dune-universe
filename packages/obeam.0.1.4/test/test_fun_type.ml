open Test_util

let%expect_test "test_fun_type.beam" =
  print_ast "test_fun_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_fun_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_fun_type))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (t 0)
              (u 0)
              (v 0)
              (w 0))))
          (DeclType
            (line 6)
            (name t)
            (tvars ())
            (ty (TyFunAny (line 6))))
          (DeclType
            (line 7)
            (name u)
            (tvars ())
            (ty (
              TyFunAnyArity
              (line     7)
              (line_any 7)
              (ret (
                TyPredef
                (line 7)
                (name integer)
                (args ()))))))
          (DeclType
            (line 8)
            (name v)
            (tvars ())
            (ty (
              TyFun
              (line        8)
              (line_params 8)
              (params ())
              (ret (
                TyPredef
                (line 8)
                (name integer)
                (args ()))))))
          (DeclType
            (line 9)
            (name w)
            (tvars ())
            (ty (
              TyFun
              (line        9)
              (line_params 9)
              (params ((
                TyPredef
                (line 9)
                (name integer)
                (args ()))))
              (ret (
                TyPredef
                (line 9)
                (name integer)
                (args ()))))))
          FormEof)))) |}]
