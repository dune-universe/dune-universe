open Ppxlib

let make_require_string str =
  Printf.sprintf "require('%s')" str

let expand ~ctxt payload =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let req = make_require_string payload in
  Ast_builder.Default.eapply
    ~loc:loc
    (Ast_builder.Default.pexp_ident ~loc:loc ({txt = Longident.parse "Js.Unsafe.js_expr"; loc=loc}))
    [Ast_builder.Default.estring ~loc:loc req]


let node_of_ocaml =
  Extension.V3.declare
    "require"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let rule = Ppxlib.Context_free.Rule.extension node_of_ocaml

let () =
  Driver.register_transformation
  ~rules:[rule]
  "require"
