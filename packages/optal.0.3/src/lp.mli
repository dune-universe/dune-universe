open Common_types

type error =
  | Not_a_number
  | Unsolvable_problem of cmp * float
  | Not_a_linear_constraint of cmp

exception Error of error

type label = string

type expr = (Expr.var*float) list

type constr =
  { label : label option;
    expr : expr;
    bound_type : cmp;
    bound : float; }

type bound =
  { var : Expr.var;
    lower_bound : float;
    upper_bound : float; }

type section =
  | General
  | Integer
  | Binary

type t =
  { objective : direction;
    objective_expr : expr;
    objective_label : label option;
    constraints : constr list;
    bounds : bound list;
    types : (section * Expr.var list) list; }

val extract_bounds : constr list -> constr list * bound list
val merge_bounds : bound list -> bound list

val print_file : Buffer.t -> t -> unit

val output_file : out_channel -> t -> unit

val report_error : Format.formatter -> error -> unit
