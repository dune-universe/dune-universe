open Expr

let smart_neg e
    =
  match e with
  | Expr_const c -> Expr_const (-. c)
  | Expr_unop(Unop_neg, e1) -> e1
  | _ -> Expr_unop(Unop_neg, e)

let smart_mul e1 e2
    =
  match e1, e2 with
  | Expr_const 0.0, _ -> Expr_const 0.0
  | _, Expr_const 0.0 -> Expr_const 0.0
  | Expr_const 1.0, _ -> e2
  | _, Expr_const 1.0 -> e1
  | Expr_const -1.0, _ -> smart_neg e2
  | _, Expr_const -1.0 -> smart_neg e1
  | _ -> Expr_binop(Binop_mul, e1, e2)

let smart_div e1 e2
    =
  match e1, e2 with
  | Expr_const 0.0, _ -> Expr_const 0.0
  | _, Expr_const 1.0 -> e1
  | _ -> Expr_binop(Binop_div, e1, e2)

let smart_add e1 e2
    =
  match e1, e2 with
  | Expr_const 0.0, _ -> e2
  | _, Expr_const 0.0 -> e1
  | _ -> Expr_binop(Binop_add, e1, e2)

let smart_sub e1 e2
    =
  match e1, e2 with
  | Expr_const 0.0, _ -> smart_neg e2
  | _, Expr_const 0.0 -> e1
  | _ ->
      if e1 = e2 then
        Expr_const(0.0)
      else
        Expr_binop(Binop_sub, e1, e2)

let smart_min e1 e2
    =
  if e1 = e2 then e1 else Expr_binop(Binop_min, e1, e2)

let smart_max e1 e2
    =
    if e1 = e2 then e1 else Expr_binop(Binop_max, e1, e2)

let smart_le e1 e2 =
  if e1 = e2 then Expr_const 1.0 else Expr_binop(Binop_le, e1, e2)

let smart_ge e1 e2 =
  if e1 = e2 then Expr_const 1.0 else Expr_binop(Binop_ge, e1, e2)

let smart_lt e1 e2 =
  if e1 = e2 then Expr_const 0.0 else Expr_binop(Binop_lt, e1, e2)

let smart_gt e1 e2 =
  if e1 = e2 then Expr_const 0.0 else Expr_binop(Binop_gt, e1, e2)

let smart_abs e1
    =
  Expr_unop(Unop_abs, e1)

let smart_index_eq_ne i1 i2 e1 e2 =
  if i1 = i2 then e1 else
  if e1 = e2 then e1 else
  Expr_index_eq_ne(i1, i2, e1, e2)

let smart_unop (u: unop) (e: expr): expr =
  match u with
  | Unop_neg -> smart_neg e
  | Unop_abs -> smart_abs e

let smart_binop (b: binop) (e1: expr) (e2: expr): expr =
  match b with
  | Binop_add -> smart_add e1 e2
  | Binop_sub -> smart_sub e1 e2
  | Binop_mul -> smart_mul e1 e2
  | Binop_div -> smart_div e1 e2
  | Binop_min -> smart_min e1 e2
  | Binop_max -> smart_max e1 e2
  | Binop_le -> smart_le e1 e2
  | Binop_ge -> smart_ge e1 e2
  | Binop_lt -> smart_lt e1 e2
  | Binop_gt -> smart_gt e1 e2

let smart_if (e1: expr) (e2: expr) (e3: expr): expr =
  if e2 = e3 then e2 else
    match e1 with
    | Expr_const 0.0 -> e3
    | Expr_const _ -> e2
    | _ -> Expr_if(e1, e2, e3)

let rec simplify_expr (e: expr)
    =
  match e with 
  | Expr_const _ -> e
  | Expr_ref(_,_) -> e
  | Expr_unop(u, e1) -> smart_unop u (simplify_expr e1)
  | Expr_binop(b, e1, e2) -> smart_binop b (simplify_expr e1) (simplify_expr e2)
  | Expr_if(e1, e2, e3) -> smart_if (simplify_expr e1) (simplify_expr e2) (simplify_expr e3)
  | Expr_index_eq_ne(i1,i2,e1,e2) ->
      let e1' = map_subscripts_in_expr (Utils.subst i2 i1) e1 in
      smart_index_eq_ne i1 i2 (simplify_expr e1') (simplify_expr e2)


let rec smart_int_pow (e: expr) (n : int) : expr =
  if n == 0 then
    Expr_const 1.
  else
    if n > 0 then
      smart_mul e (smart_int_pow e (n - 1))
    else
      smart_div (Expr_const 1.) (smart_int_pow e (- n))
