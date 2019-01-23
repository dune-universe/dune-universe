open Test_util

let%expect_test "test_record.beam" =
  print_ast "test_record.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_record.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_record))
          (DeclRecord
            (line 4)
            (fields (
              (4 a
                ()
                ())
              (5 b
                ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    5)
                      (integer 42)))))
                ())
              (6 c
                ()
                ((
                  TyPredef
                  (line 6)
                  (name string)
                  (args ()))))
              (7 d
                ((
                  ExprLit (
                    lit (
                      LitInteger
                      (line    7)
                      (integer 57)))))
                ((
                  TyPredef
                  (line 7)
                  (name integer)
                  (args ())))))))
          FormEof)))) |}]
