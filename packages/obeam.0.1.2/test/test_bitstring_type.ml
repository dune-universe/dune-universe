open Test_util

let%expect_test "test_bitstring_type.beam" =
  print_ast "test_bitstring_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile 1 test_bitstring_type.erl 1)
          (AttrMod 1 test_bitstring_type)
          (AttrExportType 3 (
            (t 0)
            (u 0)
            (v 0)
            (w 0)))
          (DeclType 6 t
            ()
            (TyBitstring 6
              (TyLit (LitInteger 6 0))
              (TyLit (LitInteger 6 0))))
          (DeclType 7 u
            ()
            (TyBitstring 7
              (TyLit (LitInteger 7 1))
              (TyLit (LitInteger 7 0))))
          (DeclType 8 v
            ()
            (TyBitstring 8
              (TyLit (LitInteger 8 0))
              (TyLit (LitInteger 8 2))))
          (DeclType 9 w
            ()
            (TyBitstring 9
              (TyLit (LitInteger 9 3))
              (TyLit (LitInteger 9 4))))
          FormEof)))) |}]
