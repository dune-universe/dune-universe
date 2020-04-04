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
    | [{ pvb_pat; pvb_expr; pvb_attributes = _; pvb_loc = _ }] ->
      [%expr match [%e pvb_expr] with
          | [%p pvb_pat] -> [%e expression]
          | _ -> () ]
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
