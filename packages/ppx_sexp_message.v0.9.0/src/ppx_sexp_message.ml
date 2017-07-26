open Ppx_core
open Ast_builder.Default

let sexp_atom ~loc x = [%expr Sexplib.Sexp.Atom [%e x]]
let sexp_list ~loc x = [%expr Sexplib.Sexp.List [%e x]]

let sexp_inline ~loc l =
  match l with
  | [x] -> x
  | _   -> sexp_list ~loc (elist ~loc l)
;;

(* Same as Ppx_sexp_value.omittable_sexp *)
type omittable_sexp =
  | Present of expression
  | Optional of Location.t * expression * (expression -> expression)
  | Absent

let wrap_sexp_if_present omittable_sexp ~f =
  match omittable_sexp with
  | Optional (loc, e, k) -> Optional (loc, e, (fun e -> f (k e)))
  | Present e -> Present (f e)
  | Absent -> Absent

let sexp_of_constraint ~loc expr ctyp =
  match ctyp with
  | [%type: [%t? ty] sexp_option] ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ty in
    Optional (loc, expr, fun expr -> eapply ~loc sexp_of [expr])
  | _ ->
    let sexp_of = Ppx_sexp_conv_expander.Sexp_of.core_type ctyp in
    Present (eapply ~loc sexp_of [expr])
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

let rewrite_here e =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "here"; _ }, PStr []) ->
    Ppx_here_expander.lift_position_as_string ~loc:e.pexp_loc
  | _ -> e
;;

let sexp_of_expr e =
  let e = rewrite_here e in
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_constant (Pconst_string ("", _)) ->
    Absent
  | Pexp_constant const ->
    Present (sexp_of_constant ~loc const)
  | Pexp_constraint (expr, ctyp) ->
    sexp_of_constraint ~loc expr ctyp
  | _ -> Present [%expr Sexplib.Conv.sexp_of_string [%e e]]
;;

let sexp_of_labelled_expr (label, e) =
  let loc = e.pexp_loc in
  match label, e.pexp_desc with
  | Nolabel, Pexp_constraint (expr, _) ->
    let expr_str = Pprintast.string_of_expression expr in
    let k e = sexp_inline ~loc [sexp_atom ~loc (estring ~loc expr_str); e] in
    wrap_sexp_if_present (sexp_of_expr e) ~f:k
  | Nolabel, _ ->
    sexp_of_expr e
  | Labelled "_", _ ->
    sexp_of_expr e
  | Labelled label, _ ->
    let k e =
      sexp_inline ~loc [sexp_atom ~loc (estring ~loc label); e]
    in
    wrap_sexp_if_present (sexp_of_expr e) ~f:k
  | Optional _, _ ->
    (* Could be used to encode sexp_option if that's ever needed. *)
    Location.raise_errorf ~loc
      "ppx_sexp_value: optional argument not allowed here"
;;

let sexp_of_labelled_exprs ~loc labels_and_exprs =
  let l = List.map labels_and_exprs ~f:sexp_of_labelled_expr in
  let res =
    List.fold_left (List.rev l) ~init:(elist ~loc []) ~f:(fun acc e ->
      match e with
      | Absent -> acc
      | Present e -> [%expr [%e e] :: [%e acc] ]
      | Optional (_, v_opt, k) ->
        (* We match simultaneously on the head and tail in the generated code to avoid
           changing their respective typing environments. *)
        [%expr
          match [%e v_opt], [%e acc] with
          | None, tl -> tl
          | Some v, tl -> [%e k [%expr v]] :: tl
        ])
  in
  let has_optional_values =
    List.exists l ~f:(function (Optional _ : omittable_sexp) -> true | _ -> false)
  in
  (* The two branches do the same thing, but when there are no optional values, we can do
     it at compile-time, which avoids making the generated code ugly. *)
  if has_optional_values
  then
    [%expr
      match [%e res] with
      | [h] -> h
      | [] | _ :: _ :: _ as res -> [%e sexp_list ~loc [%expr res]]
    ]
  else
    match res with
    | [%expr [ [%e? h] ] ] -> h
    | _ -> sexp_list ~loc res
;;

let expand ~loc ~path:_ = function
  | None ->
    sexp_list ~loc (elist ~loc [])
  | Some e ->
    let loc = e.pexp_loc in
    let labelled_exprs =
      match e.pexp_desc with
      | Pexp_apply (f, args) ->
        (Nolabel, f) :: args
      | _ ->
        (Nolabel, e) :: []
    in
    sexp_of_labelled_exprs ~loc labelled_exprs
;;

let message =
  Extension.declare "message" Extension.Context.expression
    Ast_pattern.(map (single_expr_payload __) ~f:(fun f x -> f (Some x)) |||
                 map (pstr nil              ) ~f:(fun f   -> f None))
    expand
;;

let () =
  Ppx_driver.register_transformation "sexp_message"
    ~extensions:[ message ]
;;
