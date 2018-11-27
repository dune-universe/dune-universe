type range = Syntax_tree.range
type expr = Expr.expr
type linkage =
  | Linkage_public
  | Linkage_private
  | Linkage_extern
type representation = Syntax_tree.representation
type unit_ = Unit.unit_

type variable = {
  variable_linkage: linkage;
  variable_dimensions: string list;
  variable_representation: representation;
  variable_unit: unit_;
}

type lhs =
  | Lhs_global of string * string list
  | Lhs_local of string * representation

type statement =
| Statement_assign of lhs * expr
| Statement_increment of lhs * expr
| Statement_scale of lhs * expr
| Statement_for of string * string * statement
| Statement_block of step list
and step =
| Step_let of string * representation * unit_ * expr
| Step_do of statement

type procedure = {
  procedure_index_args : string list;
  procedure_value_args : (string * representation * unit_) list;
  procedure_return_value : (expr * representation * unit_) option;
  procedure_body : step list;
}

type module_ = {
  module_ranges : (string * range) list;
  module_variables : (string * variable) list;
  module_procedures : (string * procedure) list;
}


exception Range_not_found of string

let module_find_range (m : module_) (range_name : string) : range =
  try List.assoc range_name m.module_ranges with Not_found -> raise (Range_not_found(range_name))

exception Variable_not_found of string

let module_find_variable (m : module_) (variable_name : string) : variable =
  try List.assoc variable_name m.module_variables with Not_found -> raise (Variable_not_found(variable_name))

let rec nested_for
    ~(subscripts : (string * string) list)
    ~(body : statement)
    : statement
    =
  match subscripts with
  | [] -> body
  | (var_name, range_name) :: rest -> Statement_for(var_name, range_name,
                                                    nested_for ~subscripts:rest ~body:body)
