[%%metapackage stdcompat]
[%%metaflag "-open", "Stdcompat"]

let () =
  assert
    ([%meta
      Ppxlib.Ast_helper.Exp.tuple (List.init 4 (fun i ->
        [%e ([%meta Metapp_preutils.Exp.of_int i], false)]))]
      = ((0, false), (1, false), (2, false), (3, false)))

[%%metadef
  let param modname k =
    [%expr [%meta Metapp_preutils.Exp.of_string modname] ::
     [%meta k ()]]]

let () =
  assert
    ([%meta (param "U" (fun _tiu -> param "V" (fun _tiv -> [%expr []])))]
       = ["U"; "V"])
