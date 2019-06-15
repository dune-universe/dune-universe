open Test_util

let%expect_test "test_map_type.beam" =
  print_ast "test_map_type.beam";
  [%expect {|
    (Ok (
      AbstractCode (
        ModDecl (
          (AttrFile
            (line      1)
            (file      test_map_type.erl)
            (file_line 1))
          (AttrMod
            (line        1)
            (module_name test_map_type))
          (AttrExportType
            (line 3)
            (type_arity_list (
              (any_map 0)
              (ab_map  0))))
          (DeclType
            (line 6)
            (name any_map)
            (tvars ())
            (ty (TyAnyMap (line 6))))
          (DeclType
            (line 9)
            (name ab_map)
            (tvars ())
            (ty (
              TyMap
              (line 9)
              (assocs (
                (TyAssocExact
                  (line 9)
                  (key (
                    TyLit (
                      lit (
                        LitAtom
                        (line 9)
                        (atom a)))))
                  (value (
                    TyPredef
                    (line 9)
                    (name integer)
                    (args ()))))
                (TyAssoc
                  (line 9)
                  (key (
                    TyLit (
                      lit (
                        LitAtom
                        (line 9)
                        (atom b)))))
                  (value (
                    TyPredef
                    (line 9)
                    (name atom)
                    (args ())))))))))
          FormEof)))) |}]
