open Expr
open Syntax_tree
open Delta
open Simplify
open Fresh

exception Impossible of string

let collect_subscript_comparisons (e: expr) : (string * string) list
    =
  let rec collect e =
    match e with
    | Expr_const _ -> []
    | Expr_ref(_,_) -> []
    | Expr_unop(_, e1) -> collect e1
    | Expr_binop(_, e1, e2) -> collect e1 @ collect e2
    | Expr_if(e1, e2, e3) -> collect e1 @ collect e2 @ collect e3
    | Expr_index_eq_ne(i1, i2, e1, e2) -> [i1,i2] @ collect e1 @ collect e2
  in
  Utils.nub_list (collect e)

let assume_subscripts_equal_in_expr (i1: string) (i2: string) (e: expr) : expr
    =
  map_subscripts_in_expr (Utils.subst i2 i1) e

let assume_subscripts_not_equal_in_expr (i1: string) (i2: string) (e: expr) : expr
    =
  let rec transform e =
    match e with
    | Expr_const _ -> e
    | Expr_ref(_variable_name, _subscripts) -> e
    | Expr_unop(u, e1) -> Expr_unop(u, transform e1)
    | Expr_binop(b, e1, e2) -> Expr_binop(b, transform e1, transform e2)
    | Expr_if(e1, e2, e3) -> Expr_if(transform e1, transform e2, transform e3)
    | Expr_index_eq_ne(n1, n2, e1, e2) ->
      if (n1,n2) = (i1,i2) || (n1,n2) = (i2,i1) then
        transform e2
      else
        Expr_index_eq_ne(n1, n2, transform e1, transform e2)
  in
  transform e

let expr_simplifies_to_zero_when_subscripts_not_equal e (i1, i2) : bool
    =
  simplify_expr (assume_subscripts_not_equal_in_expr i1 i2 e) = Expr_const 0.0


let compare_by_rank (rank: (string, int) Hashtbl.t) (var1: string) (var2: string): int
    =
  compare (Hashtbl.find rank var1) (Hashtbl.find rank var2)

type update = {
    update_loop_subscripts: (string * string) list;
    update_delta_expr: expr;
    update_delta: delta;
  }

let compute_update (s: specification) (g: fresh_name_generator) (deltas: delta list) (propagatee_variable_name: string): update
    =
  let propagatee: variable = specification_find_variable s propagatee_variable_name in
  let propagatee = rename_subscripts_in_variable g propagatee in
  begin match propagatee.variable_definition with
  | Definition_given -> raise (Impossible "propagate delta to given")
  | Definition_expr d ->
      let propagatee_delta_expr = simplify_expr (compute_expr_delta ~deltas d.definition_expr_summee) in
      let loop_subscripts = propagatee.variable_subscripts @ d.definition_expr_summation_subscripts in
      let preferred_subst_direction = List.rev (List.map fst loop_subscripts) in
      let align_with_preferred_subst_direction (i1, i2) =
        match List.find (fun x -> x == i1 || x == i2) preferred_subst_direction with
        | x when x = i1 -> i1, i2
        | x when x = i2 -> i2, i1
        | _ -> raise (Impossible "align_with_preferred_subst_direction -- neither variable found")
      in
      let equivalences =
        propagatee_delta_expr |> collect_subscript_comparisons
                              |> List.map align_with_preferred_subst_direction
                              |> List.filter (expr_simplifies_to_zero_when_subscripts_not_equal propagatee_delta_expr)
                              |> Utils.nub_list
      in
      let rename_subscript = Utils.fix (Utils.subst_assoc equivalences) in

      let should_keep_loop (i, _range_name): bool
          = not (List.mem_assoc i equivalences)
      in

      let subscripts_to_loop: (string * string) list =
        List.filter should_keep_loop loop_subscripts
      in

      let propagatee_delta_expr =
        propagatee_delta_expr |> map_subscripts_in_expr rename_subscript |> simplify_expr
      in

      let propagatee_delta_value_name =
        fresh_name_generator_generate_name g ~base_name:(propagatee_variable_name ^"_delta")
      in

      let propagatee_modified_index =
        propagatee.variable_subscripts |> List.map fst |> List.map rename_subscript
      in

      let propagatee_delta = { delta_variable_name = propagatee_variable_name;
                               delta_variable = propagatee;
                               delta_variable_subscripts = propagatee_modified_index;
                               delta_amount = propagatee_delta_value_name;
                             }
      in

      { update_loop_subscripts = subscripts_to_loop;
        update_delta_expr = propagatee_delta_expr;
        update_delta = propagatee_delta; }
  end
