open Test_util

let%expect_test "test_operator_type.beam" =
  print_ast "test_operator_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_operator_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_operator_type))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (t 0)
              (u 0)
              (v 0))))
          (DeclType
            (line 6)
            (name t)
            (tvars ())
            (ty (
              TyBinOp
              (line 6)
              (op   +)
              (lhs (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 1)))))
              (rhs (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 2))))))))
          (DeclType
            (line 7)
            (name u)
            (tvars ())
            (ty (
              TyBinOp
              (line 7)
              (op   *)
              (lhs (
                TyLit (
                  lit (
                    LitInteger
                    (line    7)
                    (integer 3)))))
              (rhs (
                TyLit (
                  lit (
                    LitInteger
                    (line    7)
                    (integer 4))))))))
          (DeclType
            (line 8)
            (name v)
            (tvars ())
            (ty (
              TyUnaryOp
              (line 8)
              (op   -)
              (operand (
                TyLit (
                  lit (
                    LitInteger
                    (line    8)
                    (integer 4))))))))
          FormEof)))) |}]
