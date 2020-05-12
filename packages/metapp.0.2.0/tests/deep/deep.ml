[%%metapackage stdcompat]
[%%metaflag "-open", "Stdcompat"]

let () =
  assert
    ([%meta
      Ast_helper.Exp.tuple (List.init 4 (fun i ->
        [%e ([%meta Metapp_preutils.Exp.of_int i], false)]))]
      = ((0, false), (1, false), (2, false), (3, false)))
