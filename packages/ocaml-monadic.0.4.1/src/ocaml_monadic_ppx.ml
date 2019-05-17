open Migrate_parsetree;;
open OCaml_406.Ast;;
open Ast_mapper;;
open Asttypes;;
open Parsetree;;

let ocaml_monadic_mapper =
  (* We override the expr mapper to catch bind and orzero.  *)
  { default_mapper with
    expr = fun mapper outer_expr ->
      match outer_expr with
      | [%expr [%bind [%e? expr]]] ->
        (* Matches "bind"-annotated expressions. *)
        begin
          match expr.pexp_desc with
          | Pexp_let(Nonrecursive, value_bindings, body) ->
            (* This is a let%bind expression!  It's of the form
                 let%bind $p1 = $e1 and ... and $pn = $en in $e0
               and we want it to take the form
                 bind $e1 (fun $p1 -> ... bind $en (fun $pn -> ...) ...)
            *)
            let rec bind_wrap value_bindings' =
              match value_bindings' with
              | { pvb_pat = bind_pattern
                ; pvb_expr = bind_expr
                ; pvb_attributes = []
                ; pvb_loc = _bind_loc
                }::value_bindings'' ->
                (* Recurse and then wrap the resulting body. *)
                let body' = bind_wrap value_bindings'' in
                let cont_function =
                  [%expr fun [%p bind_pattern] -> [%e body']]
                    [@metaloc expr.pexp_loc]
                in
                [%expr
                  bind [%e mapper.expr mapper bind_expr] [%e cont_function]]
                  [@metaloc expr.pexp_loc]
              | _ ->
                (* Nothing left to do.  Just return the body. *)
                mapper.expr mapper body
            in
            bind_wrap value_bindings
          | Pexp_match (expr_match, cases) ->
            let f = Ast_helper.Exp.function_ cases in
            mapper.expr mapper ([%expr bind [%e expr_match] [%e f]] [@metaloc expr.pexp_loc])
          | Pexp_ifthenelse (expr_if, expr_then, expr_else) ->
            let expr_else =
              match expr_else with
              | None -> [%expr ()]
              | Some case -> case
            in
            let cases =
              [ Ast_helper.Exp.case [%pat? true] expr_then
              ; Ast_helper.Exp.case [%pat? false] expr_else ]
            in
            let f = Ast_helper.Exp.function_ cases in
            mapper.expr mapper ([%expr bind [%e expr_if] [%e f]] [@metaloc expr.pexp_loc])
          | Pexp_sequence (expr_seq_l, expr_seq_r) ->
            mapper.expr mapper ([%expr bind [%e expr_seq_l] (fun () -> [%e expr_seq_r])] [@metaloc expr.pexp_loc])
          | _ -> default_mapper.expr mapper outer_expr
        end
      | [%expr [%orzero [%e? expr]]] ->
        (* Matches "orzero"-annotated expressions. *)
        begin
          match expr.pexp_desc with
          | Pexp_let(Nonrecursive, value_bindings, body) ->
            (* This is a let%orzero expression.  It's of the form
                 let%orzero $p1 = $e1 and ... and $pn = $en in $e0
               and we want it to take the form
                 match $e1 with
                 | $p1 -> (match $e2 with
                           | $p2 -> ...
                                    (match $en with
                                     | $pn -> $e0
                                     | _ -> zero ())
                           | _ -> zero ())
                 | _ -> zero ()
            *)
            let rec orzero_wrap value_bindings' =
              match value_bindings' with
              | { pvb_pat = orzero_pattern
                ; pvb_expr = orzero_expr
                ; pvb_attributes = []
                ; pvb_loc = _orzero_loc
                }::value_bindings'' ->
                (* Recurse and then wrap the resulting body. *)
                let body' = orzero_wrap value_bindings'' in
                [%expr
                  match [%e mapper.expr mapper orzero_expr] with
                  | [%p orzero_pattern] -> [%e body']
                  | _ -> zero ()
                ]
                  [@metaloc expr.pexp_loc]
              | _ ->
                (* Nothing left to do.  Just return the body. *)
                mapper.expr mapper body
            in
            orzero_wrap value_bindings
          | _ -> default_mapper.expr mapper outer_expr
        end
      | [%expr [%guard [%e? guard_expr]]; [%e? body_expr]] ->
        (* This is a sequenced expression with a [%guard ...] extension.  It
           takes the form
             [%guard expr']; expr
           and we want it to take the form
             if expr' then expr else zero ()
        *)
        mapper.expr mapper
          [%expr if [%e guard_expr] then [%e body_expr] else zero ()]
          [@metaloc outer_expr.pexp_loc]
      | _ -> default_mapper.expr mapper outer_expr
  }
;;
