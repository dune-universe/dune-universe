open Ppxlib

let name = "there"

let extension =
  Extension.V3.declare
    name
    Extension.Context.expression
    Ast_pattern.(pstr nil)
    (fun ~ctxt ->
       let loc = Expansion_context.Extension.extension_point_loc ctxt in
       let code_path = Expansion_context.Extension.code_path ctxt in
       Ast_builder.Default.estring ~loc (Code_path.fully_qualified_path code_path)
    )

let rule = Context_free.Rule.extension extension

let () =
  Ppxlib.Driver.register_transformation
    ~rules:[rule]
    name
