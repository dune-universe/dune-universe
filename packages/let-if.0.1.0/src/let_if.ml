open Ppxlib
open Ast_builder.Default

let expand_let ~let_loc:loc
    rec_flag
    (bindings : Ppxlib.value_binding list)
    (expression : Ppxlib.expression) 
  : Ppxlib.expression =
  match rec_flag with 
  | Recursive ->
    Location.raise_errorf ~loc "[%%if ] cannot apply to a recursive let binding"
  | Nonrecursive ->
    match bindings with
    | [{ pvb_pat; pvb_expr; pvb_attributes; pvb_loc }] ->
      let match_expression =
        pexp_match ~loc
          pvb_expr
          [ { pc_lhs = pvb_pat;
              pc_guard = None;
              pc_rhs = expression };
            { pc_lhs = ppat_any loc;
              pc_guard = None;
              pc_rhs = [%expr ()]; }] in
      { match_expression with
        pexp_attributes =
          [{ loc; txt = "ocaml.warning" },
           PStr [ pstr_eval ~loc (estring ~loc "-11") [] ] ]}

    | _ ->
      Location.raise_errorf ~loc "[%%if ] cannot apply to let-and bindings"

let expand_let ~loc ~path:_ e =
  Ast_pattern.parse
    Ast_pattern.(pexp_let __ __ __) loc e ~on_error:(fun () ->
        Location.raise_errorf ~loc "[%%if ] must apply to a let statement")
    (expand_let ~let_loc:e.pexp_loc)

let let_if : Extension.t =
  Extension.declare "if"
    Extension.Context.Expression
    Ast_pattern.(single_expr_payload __)
    expand_let

let () =
  Ppxlib.Driver.register_transformation "let-if"
    ~extensions:[let_if]
