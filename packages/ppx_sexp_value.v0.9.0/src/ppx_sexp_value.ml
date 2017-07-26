open Ppx_core
open Ast_builder.Default

let allow_deprecated_syntax = ref false

let sexp_atom ~loc x = [%expr Sexplib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Sexplib.Sexp.List [%e x]]

let rec list_and_tail_of_ast_list rev_el e =
  match e.pexp_desc with
  | Pexp_construct ({ txt = Lident "::"; _ },
                    Some { pexp_desc = Pexp_tuple [hd; tl]; _ }) ->
    list_and_tail_of_ast_list (hd :: rev_el) tl
  | Pexp_construct ({ txt = Lident "[]"; _ }, None) -> List.rev rev_el, None
  | _ -> List.rev rev_el, Some e
;;

let sexp_of_constant ~loc const =
  let f typ =
    eapply ~loc (evar ~loc ("Sexplib.Conv.sexp_of_" ^ typ)) [pexp_constant ~loc const]
  in
  match const with
  | Pconst_integer   _ -> f "int"
  | Pconst_char      _ -> f "char"
  | Pconst_string    _ -> f "string"
  | Pconst_float     _ -> f "float"
;;

type omittable_sexp =
  | Present of expression
  | Optional of Location.t * expression * (expression -> expression)
  (* Optional (_, e, k) means [e] is an ast whose values have type ['a option]. The None
     case should not be displayed, and the [a] in the Some case should be displayed by
     calling k on it. *)

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Optional (loc, e, k) -> Optional (loc, e, (fun e -> f (k e)))
  | Present e -> Present (f e)

let sexp_of_constraint ~loc expr ctyp =
  match ctyp with
  | [%type: [%t? ty] sexp_option] ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    Optional (loc, expr, fun expr -> eapply ~loc sexp_of [expr])
  | _ ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
    Present (eapply ~loc sexp_of [expr])
;;

let rec sexp_of_expr expr =
  match omittable_sexp_of_expr expr with
  | Present v -> v
  | Optional (loc, _, _) ->
    Location.raise_errorf ~loc
      "ppx_sexp_value: cannot handle sexp_option in this context"

and omittable_sexp_of_expr expr =
  let loc = expr.pexp_loc in
  wrap_sexp_if_present ~f:(fun new_expr ->
    { new_expr with pexp_attributes = expr.pexp_attributes })
    (match expr.pexp_desc with
     | Pexp_ifthenelse (e1, e2, e3) ->
       Present
         { expr with
           pexp_desc =
             Pexp_ifthenelse (e1, sexp_of_expr e2,
                              match e3 with
                              | None -> None
                              | Some e -> Some (sexp_of_expr e))
         }
     | Pexp_constraint (expr, ctyp) ->
       sexp_of_constraint ~loc expr ctyp
     | Pexp_construct ({ txt = Lident "[]"; _ }, None)
     | Pexp_construct ({ txt = Lident "::"; _ },
                       Some { pexp_desc = Pexp_tuple [_; _]; _ }) ->
       let el, tl = list_and_tail_of_ast_list [] expr in
       let el = List.map el ~f:omittable_sexp_of_expr in
       let tl =
         match tl with
         | None -> [%expr [] ]
         | Some e ->
           [%expr
             match [%e sexp_of_expr e] with
             | Sexplib.Sexp.List l -> l
             | Sexplib.Sexp.Atom _ as sexp -> [sexp]
           ]
       in
       Present (sexp_of_omittable_sexp_list loc el ~tl)
     | Pexp_constant const ->
       Present (sexp_of_constant ~loc const)
     | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
       Present (sexp_atom ~loc (Ppx_here_expander.lift_position_as_string ~loc))
     | Pexp_construct ({ txt = Lident "()"; _ }, None) ->
       Present (sexp_list ~loc (elist ~loc []))
     | Pexp_construct ({ txt = Lident constr; _ }, None)
     | Pexp_variant   (               constr     , None) ->
       Present (sexp_atom ~loc (estring ~loc constr))
     | Pexp_construct ({ txt = Lident constr; _ }, Some arg)
     | Pexp_variant   (               constr     , Some arg) ->
       let k hole =
         sexp_list ~loc
           (elist ~loc [ sexp_atom ~loc (estring ~loc constr)
                       ; hole
                       ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr arg) ~f:k
     | Pexp_tuple el ->
       let el = List.map el ~f:omittable_sexp_of_expr in
       Present (sexp_of_omittable_sexp_list loc el ~tl:(elist ~loc []))
     | Pexp_record (fields, None) ->
       Present (sexp_of_record ~loc fields)
     | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "~~"; _ }; _},
                   [ (Nolabel, { pexp_desc = Pexp_constraint (expr, ctyp); _ }) ]) ->
       let expr_str = Pprintast.string_of_expression expr in
       let k hole =
         sexp_list ~loc
           (elist ~loc [ sexp_atom ~loc (estring ~loc expr_str)
                       ; hole
                       ])
       in
       wrap_sexp_if_present (sexp_of_constraint ~loc expr ctyp) ~f:k
     | _ ->
       Location.raise_errorf ~loc
         "ppx_sexp_value: don't know how to handle this construct"
    )

and sexp_of_omittable_sexp_list loc el ~tl =
  let l =
    List.fold_left (List.rev el) ~init:tl ~f:(fun acc e ->
      match e with
      | Present e -> [%expr [%e e] :: [%e acc] ]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc ] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl
        ])
  in
  sexp_list ~loc l

and sexp_of_record ~loc fields =
  sexp_of_omittable_sexp_list loc ~tl:(elist ~loc [])
    (List.map fields ~f:(fun (id, e) ->
       let loc = { id.loc with loc_end = e.pexp_loc.loc_end } in
       let name = String.concat ~sep:"." (Longident.flatten_exn id.txt) in
       let k hole =
         sexp_list ~loc (elist ~loc [ sexp_atom ~loc (estring ~loc:id.loc name); hole ])
       in
       wrap_sexp_if_present (omittable_sexp_of_expr e) ~f:k))
;;

(* Deprecated since 2015-12. *)
module Deprecated = struct
  let hint =
    "See this page for details:\n\
     https://github.com/janestreet/ppx_sexp_value"

  let rewrite_arg e =
    let loc = e.pexp_loc in
    match e.pexp_desc with
    | Pexp_constraint _ -> eapply ~loc (evar ~loc "~~") [e]
    | _ -> e
  ;;

  let add_warning e msg =
    let attr = attribute_of_warning e.pexp_loc msg in
    { e with pexp_attributes = attr :: e.pexp_attributes }
  ;;

  let rewrite_expr expr =
    let loc = expr.pexp_loc in
    match expr.pexp_desc with
    (* Don't misinterpret [%sexp ~~(e : t)] for the deprecated application syntax. *)
    | Pexp_apply ({ pexp_desc = Pexp_ident { txt = Lident "~~"; _}; _ }, _) -> expr
    | Pexp_apply (f, (_ :: _ as args))
      when List.for_all args ~f:(fun (lab, _) -> match lab with Nolabel -> true | _ -> false) ->
      let el = List.map (f :: List.map args ~f:snd) ~f:rewrite_arg in
      let e = pexp_tuple ~loc el in
      if !allow_deprecated_syntax then
        e
      else
        add_warning e
          ("ppx_sexp_value: the application syntax is deprecated.\n" ^ hint)
    | _ -> expr
  ;;

  let add_deprecated e =
    if !allow_deprecated_syntax then
      e
    else
      add_warning e
        ("ppx_sexp_value: deprecated extension, use [%sexp] instead.\n" ^ hint)
  ;;

  let expand_structural_sexp ~loc:_ ~path:_ e =
    add_deprecated (sexp_of_expr (rewrite_expr e))
  ;;

  let expand_error func_name ~loc:_ ~path:_ loc e1 e2 =
    add_deprecated
      (eapply ~loc (evar ~loc func_name)
         [e1; [%expr ()]; [%expr fun () -> [%e sexp_of_expr e2]]])
  ;;
end

let expand ~loc ~path e =
  if !allow_deprecated_syntax then
    Deprecated.expand_structural_sexp ~loc ~path e
  else
    (* Rewrite to make sure we get the warning and not an error *)
    let e = Deprecated.rewrite_expr e in
    sexp_of_expr e
;;

let extensions =
  let one_expr  =
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
  and two_exprs =
    Ast_pattern.(pstr (pstr_eval (pexp_loc __ (pexp_apply __ ((no_label __) ^:: nil))) nil
                       ^:: nil))
  in
  let declare name patt k =
    Extension.declare name Extension.Context.expression patt k
  in
  [ declare "sexp"                  one_expr  expand
  ; declare "structural_sexp"       one_expr   Deprecated.expand_structural_sexp
  ; declare "raise_structural_sexp" two_exprs (Deprecated.expand_error "Error.failwiths")
  ; declare "structural_error"      two_exprs (Deprecated.expand_error "Error.create"   )
  ; declare "structural_or_error"   two_exprs (Deprecated.expand_error "Or_error.error" )
  ]
;;

let () =
  Ppx_driver.register_transformation "sexp_value"
    ~extensions
;;
