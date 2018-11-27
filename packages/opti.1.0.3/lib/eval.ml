open Expr
open Syntax_tree
open Fresh

exception Eval_phantom_given of string

let rec eval (s: specification) (f : fresh_name_generator) (e : expr) : Imperative.step list * expr =
  match e with
  | Expr_ref(variable_name, subscripts) -> begin
      let v: variable = specification_find_variable s variable_name in
      match v.variable_linkage with
      | Linkage_public | Linkage_private | Linkage_extern -> [], e
      | Linkage_phantom -> begin
          let v: variable = rename_subscripts_in_variable f v in
          match v.variable_definition with
          | Definition_given -> raise (Eval_phantom_given(variable_name))
          | Definition_expr d ->
             let index_args = v.variable_subscripts |> List.map fst in
             let renames : (string * string) list = List.combine index_args subscripts in
             let alpha_rename (name : string) = try List.assoc name renames with Not_found -> name in
             let summee = map_subscripts_in_expr alpha_rename d.definition_expr_summee in
             begin match d.definition_expr_summation_subscripts with
             | [] -> eval s f summee
             | _ ->
                let sum_variable_name = fresh_name_generator_generate_name f ~base_name:(variable_name ^ "_sum") in
                let steps =
                  [Imperative.Step_let(sum_variable_name, v.variable_representation, v.variable_unit, Expr_const(0.0));
                   Imperative.Step_do(Imperative.nested_for
                                        ~subscripts:d.definition_expr_summation_subscripts
                                        ~body: (Imperative.Statement_block(
                                                    let steps, result = eval s f summee in
                                                    steps @
                                                      [Step_do(Imperative.Statement_increment(Imperative.Lhs_local(sum_variable_name, v.variable_representation), result))])))]
                in
                let result = Expr_ref(sum_variable_name, []) in
                steps, result
             end
        end
    end
  | Expr_const c -> [], Expr_const c
  | Expr_unop(u, e1) ->
     let steps1, e1 = eval s f e1 in
     steps1, Expr_unop(u, e1)
  | Expr_binop(b, e1, e2) ->
     let steps1, e1 = eval s f e1 in
     let steps2, e2 = eval s f e2 in
     steps1 @ steps2, Expr_binop(b, e1, e2)
  | Expr_if(e1, e2, e3) ->
     let steps1, e1 = eval s f e1 in
     (* TODO short circuit? use block expr? *)
     let steps2, e2 = eval s f e2 in
     let steps3, e3 = eval s f e3 in
     steps1 @ steps2 @ steps3, Expr_if(e1, e2, e3)
  | Expr_index_eq_ne(i1, i2, e1, e2) ->
     (* TODO short circuit? use block expr? *)
     let steps1, e1 = eval s f e1 in
     let steps2, e2 = eval s f e2 in
     steps1 @ steps2, Expr_index_eq_ne(i1, i2, e1, e2)
