open! Base
open! Import

let maybe_wrap_in_submodule structure_items ~loc ~submodule =
  match submodule with
  | None -> structure_items
  | Some submodule ->
    [ module_binding
        ~loc
        ~name:(Located.mk ~loc (Some submodule))
        ~expr:(pmod_structure structure_items ~loc)
      |> pstr_module ~loc
    ]
;;

let generate_structure ~loc ~path:_ (_rec_flag, tds) submodule =
  List.concat_map tds ~f:(Fn.compose Type.to_strs Type.of_type_declaration)
  |> maybe_wrap_in_submodule ~loc ~submodule
;;

let generate_signature ~loc ~path (rec_flag, tds) submodule =
  generate_structure ~loc ~path (rec_flag, tds) submodule
  |> pmod_structure ~loc
  |> pmty_typeof ~loc
  |> include_infos ~loc
  |> psig_include ~loc
  |> List.return
;;

let args () = Deriving.Args.(empty +> arg "submodule" (pexp_construct (lident __) none))

let (_ : Deriving.t) =
  Deriving.add
    "accessors"
    ~str_type_decl:(Deriving.Generator.make (args ()) generate_structure)
    ~sig_type_decl:(Deriving.Generator.make (args ()) generate_signature)
;;

let polymorphize_extension =
  Extension.declare
    "accessor"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload __)
    (fun ~loc ~path:_ expr -> Polymorphize.polymorphize ~loc ~expr)
;;

let () = Driver.register_transformation "accessor" ~extensions:[ polymorphize_extension ]
