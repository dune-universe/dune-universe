type unop =
  | Unop_neg
  | Unop_abs

type binop =
  | Binop_add
  | Binop_sub
  | Binop_mul
  | Binop_div
  | Binop_min
  | Binop_max
  | Binop_le
  | Binop_ge
  | Binop_lt
  | Binop_gt

type expr =
  | Expr_const of float
  | Expr_ref of string * string list
  | Expr_unop of unop * expr
  | Expr_binop of binop * expr * expr
  | Expr_if of expr * expr * expr
  | Expr_index_eq_ne of string * string * expr * expr   (* if the two indices are equal, then first expr, else second expr *)

let map_subscripts_in_expr (f: string -> string) (e: expr)
    =
  let rec process e =
    match e with
    | Expr_const _ -> e
    | Expr_ref(variable_name,subscripts) -> Expr_ref(variable_name, List.map f subscripts)
    | Expr_unop(u, e1) -> Expr_unop(u, process e1)
    | Expr_binop(b, e1, e2) -> Expr_binop(b, process e1, process e2)
    | Expr_index_eq_ne(i1, i2, e1, e2) -> Expr_index_eq_ne(f i1, f i2, process e1, process e2)
    | Expr_if(e1, e2, e3) -> Expr_if(process e1, process e2, process e3)
  in
  process e
