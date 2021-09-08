open Ppxlib

let name = "matches"

let expand ~ctxt pat guard =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    Ast_builder.Default.(
        pexp_function ~loc [
            case
                ~lhs:pat
                ~guard
                ~rhs:(pexp_construct ~loc (Located.lident ~loc "true") None);
            case
                ~lhs:(ppat_any ~loc)
                ~guard:None
                ~rhs:(pexp_construct ~loc (Located.lident ~loc "false") None)
        ])

let ext =
    Extension.V3.declare
        name
        Extension.Context.expression
        Ast_pattern.(ppat __ __)
        expand

let rule = Ppxlib.Context_free.Rule.extension ext

let () = Ppxlib.Driver.register_transformation ~rules:[rule] name
