open Test_util

let%expect_test "test_bitstring_type.beam" =
  print_ast "test_bitstring_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_bitstring_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_bitstring_type))
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
            (ty (
              TyBitstring
              (line 6)
              (m (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 0)))))
              (n (
                TyLit (
                  lit (
                    LitInteger
                    (line    6)
                    (integer 0))))))))
          (DeclType
            (line 7)
            (name u)
            (tvars ())
            (ty (
              TyBitstring
              (line 7)
              (m (
                TyLit (
                  lit (
                    LitInteger
                    (line    7)
                    (integer 1)))))
              (n (
                TyLit (
                  lit (
                    LitInteger
                    (line    7)
                    (integer 0))))))))
          (DeclType
            (line 8)
            (name v)
            (tvars ())
            (ty (
              TyBitstring
              (line 8)
              (m (
                TyLit (
                  lit (
                    LitInteger
                    (line    8)
                    (integer 0)))))
              (n (
                TyLit (
                  lit (
                    LitInteger
                    (line    8)
                    (integer 2))))))))
          (DeclType
            (line 9)
            (name w)
            (tvars ())
            (ty (
              TyBitstring
              (line 9)
              (m (
                TyLit (
                  lit (
                    LitInteger
                    (line    9)
                    (integer 3)))))
              (n (
                TyLit (
                  lit (
                    LitInteger
                    (line    9)
                    (integer 4))))))))
          FormEof)))) |}]
