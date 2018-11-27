open Expr
open Syntax_tree

type delta = {
    delta_variable_name: string;
    delta_variable: variable;
    delta_variable_subscripts: string list;
    delta_amount: string;
  }

exception Delta_not_found of string

let find_delta (deltas: delta list) (variable_name: string): delta
    =
  try
    List.find (fun d -> d.delta_variable_name = variable_name) deltas
  with
    Not_found -> raise (Delta_not_found variable_name)


let compute_ref_delta (deltas: delta list) (variable_name: string) (variable_subscripts: string list): expr
    =
  try
    let d = find_delta deltas variable_name in
    let rec build_expr = function
      | [] -> Expr_ref(d.delta_amount, [])
      | (i1,i2) :: rest -> Expr_index_eq_ne(i1, i2, build_expr rest, Expr_const 0.0)
    in
    List.combine variable_subscripts d.delta_variable_subscripts |> build_expr
  with
    Delta_not_found _ -> Expr_const 0.0

let compute_expr_delta ~(deltas: delta list) (e : expr) =
  let rec process (e: expr) =
    match e with
    | Expr_const _ -> Expr_const 0.0
    | Expr_ref(ref_variable_name, ref_variable_subscripts) -> compute_ref_delta deltas ref_variable_name ref_variable_subscripts
    | Expr_index_eq_ne(i1,i2,e1,e2) -> Expr_index_eq_ne(i1, i2, process e1, process e2)
    | Expr_unop(Unop_neg, e1) -> Expr_unop(Unop_neg, process e1)
    | Expr_unop(Unop_abs, e1) ->
        let e1' = process e1 in
        Expr_binop(Binop_sub,
                   Expr_unop(Unop_abs, e1),
                   Expr_unop(Unop_abs, Expr_binop(Binop_sub, e1, e1')))
    | Expr_binop(b, e1, e2) -> begin
        let e1' = process e1 in
        let e2' = process e2 in
        match b with
        | Binop_add | Binop_sub -> Expr_binop(b, e1', e2')
        | Binop_mul ->
           Expr_binop(Binop_sub,
                      Expr_binop(Binop_add,
                                 Expr_binop(Binop_mul, e1, e2'),
                                 Expr_binop(Binop_mul, e1', e2)),
                      Expr_binop(Binop_mul, e1', e2'))
        | Binop_div ->
           (*
             (1/e2)' = 1/e2 - 1/(e2 - e2') = (-e2 + e2 - e2') / e2(e2 - e2') = -e2' / e2(e2 - e2')
            *)
           let e2_recip' = Expr_binop(Binop_div,
                                      Expr_unop(Unop_neg, e2'),
                                      Expr_binop(Binop_mul, e2, Expr_binop(Binop_sub, e2, e2'))) in
           (* Like mul above but substitute e2 -> 1/e2 *)
           Expr_binop(Binop_sub,
                      Expr_binop(Binop_add,
                                 Expr_binop(Binop_mul, e1, e2_recip'),
                                 Expr_binop(Binop_div, e1', e2)),
                      Expr_binop(Binop_mul, e1', e2_recip'))
        | Binop_min | Binop_max
          | Binop_le | Binop_ge | Binop_lt | Binop_gt ->
           let old_e1 = Expr_binop(Binop_sub, e1, e1') in
           let old_e2 = Expr_binop(Binop_sub, e2, e2') in
           Expr_binop(Binop_sub,
                      Expr_binop(b, e1, e2),
                      Expr_binop(b, old_e1, old_e2))
      end
    | Expr_if(e1, e2, e3) ->
       let old_e1 = Expr_binop(Binop_sub, e1, process e1) in
       let old_e2 = Expr_binop(Binop_sub, e2, process e2) in
       let old_e3 = Expr_binop(Binop_sub, e3, process e3) in
       Expr_binop(Binop_sub,
                  Expr_if(e1, e2, e3),
                  Expr_if(old_e1, old_e2, old_e3))
  in
  process e
