open! Base
open! Import
open Common

let polymorphize ~loc ~expr =
  let dictionary_pat, dictionary_expr = gen_symbol "dictionary" ~loc in
  let mapping_pat, mapping_expr = gen_symbol "mapping" ~loc in
  [%expr
    { Accessor.f =
        (fun [%p dictionary_pat] [%p mapping_pat] ->
           [%e expr].f [%e dictionary_expr] [%e mapping_expr])
    }]
;;

let str_binding ~loc ~name ~expr =
  pstr_value
    ~loc
    Nonrecursive
    [ value_binding ~loc ~pat:(ppat_var ~loc (Loc.make name ~loc)) ~expr ]
;;

let binding ~loc ~name ~expr = str_binding ~loc ~name ~expr:(polymorphize ~loc ~expr)
