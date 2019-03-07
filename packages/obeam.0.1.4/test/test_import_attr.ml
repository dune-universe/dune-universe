open Test_util

let%expect_test "test_import_attr.beam" =
  print_ast "test_import_attr.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_import_attr.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_import_attr))
          (AttrImport
            (line        2)
            (module_name hoge_module)
            (function_arity_list (
              (f 0)
              (g 1)
              (h 2))))
          FormEof)))) |}]
